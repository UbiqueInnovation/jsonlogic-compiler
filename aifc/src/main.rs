use colorize::AnsiColor;
use structopt::StructOpt;

#[derive(StructOpt)]
struct CompilerArgs {
    #[structopt(
        long = "output",
        short = "o",
        default_value = "a.json",
        help = "output file"
    )]
    output: String,
    #[structopt(long = "minimize", short = "m")]
    minimize: bool,
    #[structopt(name = "INPUT_FILE")]
    file_name: String,
}

fn main() {
    let args = CompilerArgs::from_args();
    let input_file = std::fs::read_to_string(args.file_name).expect("Could not read input file");
    let json_logic = match jlc::arithmetic::expression(&input_file) {
        Ok(logic) => logic,
        Err(e) => {
            let location = e.location;
            let line_num = location.line - 1;
            let lines: Vec<String> = input_file.lines().map(|s| s.to_string()).collect();
            let line = &lines[line_num];
            let mut output = String::from("\n");
            if line_num> 0 {
                output.push_str(&(lines[line_num - 1].clone().green()));
                output.push('\n');
            }
            output.push_str(&line.clone().red());
            output.push('\n');
            for _ in 0..location.column-1 {
                output.push_str(&"-".red());
            }
            output.push_str(&"^".red());
            output.push(' ');
            output.push_str(&format!("Expected: [{}]", e.expected).red());
            output.push('\n');
            if line_num < lines.len() - 1 {
                output.push_str(&(lines[line_num + 1].clone().green()));
            }
            println!("{}{}", format!("SyntaxError at {:?}", location).red(), output);
            return;
        }
    };
    if args.minimize {
        std::fs::write(args.output, json_logic.to_json_logic().to_string())
            .expect("Couldn't write output file");
    } else {
        std::fs::write(
            args.output,
            serde_json::to_string_pretty(&json_logic.to_json_logic())
                .expect("Serialization failed"),
        )
        .expect("Couldn't write output file");
    }
}
