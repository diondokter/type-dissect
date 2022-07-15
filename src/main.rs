use build_html::Html;
use clap::Parser;
use comfy_table::{Cell, Color, Table};
use gimli::{AttributeValue, DebuggingInformationEntry, Dwarf, EndianSlice, Reader, UnitHeader};
use object::{Object, ObjectSection};
use std::{
    fmt::Display,
    path::PathBuf,
    sync::atomic::{AtomicUsize, Ordering},
};

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    #[clap(short, long, value_parser)]
    type_name: String,
    #[clap(short, long, value_parser)]
    exe_path: PathBuf,
}

#[allow(dead_code)]
struct TestThing {
    a: u8,
    b: u16,
    c: u8,
}

fn main() -> Result<(), anyhow::Error> {
    let _thing = TestThing { a: 0, b: 1, c: 2 };

    let args = Args::parse();

    fern::Dispatch::new()
        .format(|out, message, record| {
            out.finish(format_args!(
                "{}[{}][{}] {}",
                chrono::Local::now().format("[%H:%M:%S]"),
                record.target(),
                record.level(),
                message
            ))
        })
        .level(log::LevelFilter::Debug)
        .chain(std::io::stdout())
        .apply()?;

    let file_bytes = std::fs::read(args.exe_path)?;
    let object_file = object::File::parse(file_bytes.as_slice())?;

    let endianness = match object_file.is_little_endian() {
        true => gimli::RunTimeEndian::Little,
        false => gimli::RunTimeEndian::Big,
    };

    let dwarf = Dwarf::load(
        |section| match object_file.section_by_name(section.name()) {
            Some(section) => {
                Result::<_, anyhow::Error>::Ok(EndianSlice::new(section.data()?, endianness))
            }
            None => Ok(EndianSlice::default()),
        },
    )?;

    let mut units = dwarf.debug_info.units();

    while let Some(unit) = units.next()? {
        let abbreviations = unit.abbreviations(&dwarf.debug_abbrev)?;

        let mut entries = unit.entries(&abbreviations);

        while let Some((_, entry)) = entries.next_dfs()? {
            if read_entry_name_attr(entry, &dwarf, &unit)? != Some(args.type_name.clone()) {
                continue;
            }

            log::info!(
                "Found possible type at {:X?}",
                entry.offset().to_debug_info_offset(&unit)
            );

            let data = match get_entry_type_data(entry, &dwarf, &unit) {
                Ok(data) => data,
                Err(e) => {
                    log::info!("Could not read type: {e}");
                    continue;
                }
            };

            println!("{data}");

            web_view::builder()
                .title("Output")
                .content(web_view::Content::Html(
                    data.to_html_table().to_html_string(),
                ))
                .invoke_handler(|_, _| Ok(()))
                .user_data(0)
                .run()?;

            return Ok(());
        }
    }

    anyhow::bail!("Could not find the type")
}

fn get_entry_type_data<R: Reader>(
    entry: &DebuggingInformationEntry<R>,
    dwarf: &Dwarf<R>,
    unit: &UnitHeader<R>,
) -> Result<TypeData, anyhow::Error> {
    match entry.tag() {
        gimli::constants::DW_TAG_base_type => get_base_type_data(entry, dwarf, unit),
        gimli::constants::DW_TAG_pointer_type => get_pointer_type_data(entry, dwarf, unit),
        gimli::constants::DW_TAG_structure_type
        | gimli::constants::DW_TAG_union_type
        | gimli::constants::DW_TAG_class_type => get_structure_type_data(entry, dwarf, unit),
        t => anyhow::bail!("Unknown tag: {t}"),
    }
}

fn get_base_type_data<R: Reader>(
    entry: &DebuggingInformationEntry<R>,
    dwarf: &Dwarf<R>,
    unit: &UnitHeader<R>,
) -> Result<TypeData, anyhow::Error> {
    let name = read_entry_name_attr(entry, dwarf, unit)?.unwrap();
    let bit_size = entry
        .attr_value(gimli::constants::DW_AT_bit_size)?
        .map(|val| val.udata_value().unwrap());

    let bit_size = match bit_size {
        Some(bit_size) => bit_size,
        None => {
            entry
                .attr_value(gimli::constants::DW_AT_byte_size)?
                .map(|val| val.udata_value().unwrap())
                .unwrap_or(0)
                * 8
        }
    };

    let bit_offset = entry
        .attr_value(gimli::constants::DW_AT_data_bit_offset)?
        .map(|val| val.udata_value().unwrap())
        .unwrap_or(0);

    Ok(TypeData {
        name,
        bit_size,
        bit_offset,
        members: Vec::new(),
    })
}

fn get_structure_type_data<R: Reader>(
    entry: &DebuggingInformationEntry<R>,
    dwarf: &Dwarf<R>,
    unit: &UnitHeader<R>,
) -> Result<TypeData, anyhow::Error> {
    let name = read_entry_name_attr(entry, dwarf, unit)?.unwrap();
    let bit_size = entry
        .attr_value(gimli::constants::DW_AT_bit_size)?
        .map(|val| val.udata_value().unwrap());

    let bit_size = match bit_size {
        Some(bit_size) => bit_size,
        None => {
            entry
                .attr_value(gimli::constants::DW_AT_byte_size)?
                .map(|val| val.udata_value().unwrap())
                .unwrap_or(0)
                * 8
        }
    };

    let bit_offset = entry
        .attr_value(gimli::constants::DW_AT_data_bit_offset)?
        .map(|val| val.udata_value().unwrap())
        .unwrap_or(0);

    let mut members = Vec::new();
    let abbreviations = dwarf.abbreviations(unit)?;
    let mut tree = unit.entries_tree(&abbreviations, Some(entry.offset()))?;
    let mut children_entries = tree.root()?.children();
    while let Some(child_entry) = children_entries.next()? {
        let child_entry = child_entry.entry();
        if child_entry.tag() != gimli::constants::DW_TAG_member {
            continue;
        }

        let member_name = read_entry_name_attr(child_entry, dwarf, unit)?.unwrap();

        let member_location = child_entry
            .attr_value(gimli::constants::DW_AT_data_member_location)?
            .map(|attr| attr.udata_value().unwrap() * 8);

        let bit_offset = child_entry
            .attr_value(gimli::constants::DW_AT_data_bit_offset)?
            .map(|attr| attr.udata_value().unwrap());

        let bit_offset = member_location.unwrap_or_else(|| bit_offset.unwrap());

        let member_type_entry_offset = child_entry
            .attr_value(gimli::constants::DW_AT_type)?
            .map(|attr| match attr {
                AttributeValue::UnitRef(offset) => offset,
                v => unreachable!("{v:?}"),
            })
            .unwrap();

        let mut member_type_data = get_entry_type_data(
            &unit.entry(&abbreviations, member_type_entry_offset)?,
            dwarf,
            unit,
        )?;

        member_type_data.bit_offset = bit_offset;
        member_type_data.name = format!("{member_name}: {}", member_type_data.name);
        members.push(member_type_data);
    }

    Ok(TypeData {
        name,
        bit_size,
        bit_offset,
        members,
    })
}

fn get_pointer_type_data<R: Reader>(
    entry: &DebuggingInformationEntry<R>,
    dwarf: &Dwarf<R>,
    unit: &UnitHeader<R>,
) -> Result<TypeData, anyhow::Error> {
    let name =
        read_entry_name_attr(entry, dwarf, unit)?.unwrap_or_else(|| "Unknown pointer".into());

    Ok(TypeData {
        name,
        bit_size: unit.address_size() as u64 * 8,
        bit_offset: 0,
        members: Vec::new(),
    })
}

fn read_entry_name_attr<R: Reader>(
    entry: &DebuggingInformationEntry<R>,
    dwarf: &Dwarf<R>,
    unit: &UnitHeader<R>,
) -> Result<Option<String>, anyhow::Error> {
    let name_attribute = match entry.attr(gimli::constants::DW_AT_name)? {
        Some(name_attribute) => name_attribute,
        None => return Ok(None),
    };

    Ok(Some(
        dwarf
            .attr_string(&dwarf.unit(unit.clone())?, name_attribute.value())?
            .to_string()?
            .to_string(),
    ))
}

#[derive(Debug)]
struct TypeData {
    name: String,
    bit_size: u64,
    bit_offset: u64,
    members: Vec<TypeData>,
}

impl TypeData {
    fn get_table_header(&self) -> Vec<Cell> {
        let mut cells = (0..self.bit_size)
            .map(|bit| Cell::new(format!("{bit:02X}")))
            .collect::<Vec<_>>();

        cells.insert(0, Cell::new("Data"));

        cells
    }

    fn get_table_row(&self, color: Color, name_indent: u32) -> Vec<Cell> {
        let mut cells = Vec::new();

        let mut name = self.name.clone();
        name.insert_str(
            0,
            String::from_iter((0..name_indent).map(|_| "- ")).as_str(),
        );

        cells.push(Cell::new(name));

        cells.extend((0..self.bit_offset).map(|_| Cell::new("")));

        cells.extend(
            (self.bit_offset..self.bit_offset + self.bit_size)
                .map(|_| Cell::new(" ").bg(color).fg(Color::White)),
        );

        cells
    }

    fn get_table_rows_recursive(&self, rows: &mut Vec<Vec<Cell>>, depth: u32) {
        const COLORS: &[Color] = &[
            Color::Blue,
            Color::Cyan,
            Color::Green,
            Color::Grey,
            Color::Magenta,
            Color::Red,
            Color::Yellow,
        ];
        static NEXT_COLOR: AtomicUsize = AtomicUsize::new(0);

        rows.push(self.get_table_row(
            COLORS[NEXT_COLOR.fetch_add(1, Ordering::SeqCst) % COLORS.len()],
            depth,
        ));

        for member in self.members.iter() {
            member.get_table_rows_recursive(rows, depth + 1);
        }
    }

    fn to_html_table(&self) -> build_html::Table {
        let mut table =
            build_html::Table::new().with_attributes([("style", "border: 1px solid black;")]);

        table.add_header_row(self.get_table_header().iter().map(|c| c.content()));

        let mut rows = Vec::new();

        self.get_table_rows_recursive(&mut rows, 0);

        for row in rows {
            table.add_body_row(row.iter().map(|c| {
                if c.content() == " " {
                    "X".to_string()
                } else {
                    c.content()
                }
            }));
        }

        table
    }
}

impl Display for TypeData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut table = Table::new();
        table
            .load_preset(comfy_table::presets::UTF8_NO_BORDERS)
            .set_content_arrangement(comfy_table::ContentArrangement::Disabled)
            .set_header(self.get_table_header());

        let mut rows = Vec::new();

        self.get_table_rows_recursive(&mut rows, 0);

        for row in rows {
            table.add_row(row);
        }

        table.discover_columns();
        table
            .column_mut(0)
            .unwrap()
            .set_constraint(comfy_table::ColumnConstraint::ContentWidth);

        write!(f, "{table}")
    }
}
