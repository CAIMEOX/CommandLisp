mod diagram;

use std::collections::HashMap;
use std::fmt;
use std::io::Write;
use std::num::ParseIntError;

#[derive(Clone)]
enum RispExp {
    Symbol(String),
    Number(i32),
    List(Vec<RispExp>),
    Command(fn(&[RispExp]) -> Result<RispExp, RispErr>),
}

impl fmt::Display for RispExp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let str = match self {
            RispExp::Symbol(s) => s.clone(),
            RispExp::Number(n) => n.to_string(),
            RispExp::List(list) => {
                let xs: Vec<String> = list.iter().map(|x| x.to_string()).collect();
                format!("({})", xs.join(","))
            }
            RispExp::Command(_) => "Function {}".to_string(),
        };

        write!(f, "{}", str)
    }
}

enum RispErr {
    Reason(String),
}

#[derive(Clone)]
struct MinecraftEnv {
    data: HashMap<String, RispExp>,
}

fn parse_eval(expr: String, env: &mut MinecraftEnv) -> Result<RispExp, RispErr> {
    let (parsed_exp, _) = parse(&tokenize(expr))?;
    println!("{}", &parsed_exp);
    let evaled_exp = eval(&parsed_exp, env)?;

    Ok(evaled_exp)
}

fn slurp_expr() -> String {
    let mut expr = String::new();

    std::io::stdin()
        .read_line(&mut expr)
        .expect("Failed to read line");

    expr
}

fn main() {
    diagram::new_diagram_test();
    let env = &mut default_env();
    loop {
        print!("🙀 >");
        std::io::stdout().flush();
        let expr = slurp_expr();
        match parse_eval(expr, env) {
            Ok(res) => println!("🔥 => {}", res),
            Err(e) => match e {
                RispErr::Reason(msg) => println!("🚨 => {}", msg),
            },
        }
    }
}

fn tokenize(expr: String) -> Vec<String> {
    expr.replace("(", " ( ")
        .replace(")", " ) ")
        .split_whitespace()
        .map(|x| x.to_string())
        .collect()
}

// After call tokenize
fn parse(tokens: &[String]) -> Result<(RispExp, &[String]), RispErr> {
    let (token, rest) = tokens
        .split_first()
        .ok_or(RispErr::Reason("could not get token".to_string()))?;

    match &token[..] {
        "(" => read_seq(rest),
        ")" => Err(RispErr::Reason("unexpected `)`".to_string())),
        _ => Ok((parse_atom(token), rest)),
    }
}

fn read_seq(tokens: &[String]) -> Result<(RispExp, &[String]), RispErr> {
    let mut res: Vec<RispExp> = vec![];
    let mut xs = tokens;
    loop {
        let (next_token, rest) = xs
            .split_first()
            .ok_or(RispErr::Reason("could not find closing `)`".to_string()))?;
        if next_token == ")" {
            print!("INIT TOKEN: ");
            for i in res.clone() {
                println!("{}", i)
            }
            return Ok((RispExp::List(res), rest)); // skip `)`, head to the token after
        }
        let (exp, new_xs) = parse(&xs)?;
        res.push(exp);
        xs = new_xs;
    }
}

fn parse_atom(token: &str) -> RispExp {
    let potential_float: Result<i32, ParseIntError> = token.parse();
    match potential_float {
        Ok(v) => RispExp::Number(v),
        Err(_) => RispExp::Symbol(token.to_string().clone()),
    }
}

fn default_env() -> MinecraftEnv {
    let mut data: HashMap<String, RispExp> = HashMap::new();
    data.insert(
        "+".to_string(),
        RispExp::Command(|args: &[RispExp]| -> Result<RispExp, RispErr> {
            let sum = parse_list_of_ints(args)?.iter().fold(0, |sum, a| sum + a);

            Ok(RispExp::Number(sum))
        }),
    );
    data.insert(
        "-".to_string(),
        RispExp::Command(|args: &[RispExp]| -> Result<RispExp, RispErr> {
            let floats = parse_list_of_ints(args)?;
            let first = *floats
                .first()
                .ok_or(RispErr::Reason("expected at least one number".to_string()))?;
            let sum_of_rest = floats[1..].iter().fold(0, |sum, a| sum + a);

            Ok(RispExp::Number(first - sum_of_rest))
        }),
    );

    MinecraftEnv { data }
}

fn parse_list_of_ints(args: &[RispExp]) -> Result<Vec<i32>, RispErr> {
    args.iter().map(|x| parse_single_int(x)).collect()
}

fn parse_single_int(exp: &RispExp) -> Result<i32, RispErr> {
    match exp {
        RispExp::Number(num) => Ok(*num),
        _ => Err(RispErr::Reason("expected a number".to_string())),
    }
}

fn eval(exp: &RispExp, env: &mut MinecraftEnv) -> Result<RispExp, RispErr> {
    match exp {
        RispExp::Symbol(k) => env
            .data
            .get(k)
            .ok_or(RispErr::Reason(format!("unexpected symbol k='{}'", k)))
            .map(|x| x.clone()),
        RispExp::Number(_a) => Ok(exp.clone()),
        RispExp::List(list) => {
            let first_form = list
                .first()
                .ok_or(RispErr::Reason("expected a non-empty list".to_string()))?;
            let arg_forms = &list[1..];
            let first_eval = eval(first_form, env)?;
            match first_eval {
                RispExp::Command(f) => {
                    let args_eval = arg_forms
                        .iter()
                        .map(|x| eval(x, env))
                        .collect::<Result<Vec<RispExp>, RispErr>>();
                    f(&args_eval?)
                }
                _ => Err(RispErr::Reason("first form must be a function".to_string())),
            }
        }
        RispExp::Command(_) => Err(RispErr::Reason("unexpected form".to_string())),
    }
}
