// use can_dbc::DBC;
use can_dbc::DBC;
use std::io;

fn main() -> io::Result<()> {
    let read_dbc = DBC::read_from_file("./examples/sample.dbc")?;
    read_dbc.write_to_file("./examples/sample_output.dbc")?;
    Ok(())
}