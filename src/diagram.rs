use std::fmt;

pub enum Mode {
    CommandBlockImpulse,
    CommandBlockRepeat,
    CommandBlockChain,
}

pub struct CommandBlock {
    mode: Mode,
    command: String,
    needs_redstone: bool,
    conditional: bool,
    name: String,
    execute_on_first_tick: bool,
    tick_delay: i32,
    should_track_output: bool,
}

pub type Node = Option<CommandBlock>;

pub struct Component {
    chains: Vec<Node>,
    height: i16,
}

pub struct Diagram {
    unit: Vec<Component>,
}

impl fmt::Display for CommandBlock {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{{}}}", self.command)
    }
}

impl fmt::Display for Diagram {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut str: String = "".to_string();
        for comp in &self.unit {
            for node in &comp.chains {
                if let Some(cb) = node {
                    str.push_str(&format!("{}", cb))
                } else {
                    str.push_str("{}")
                }
            }
            str.push_str("\n")
        }
        write!(f, "{}", str)
    }
}

pub fn new_diagram_test() {
    let diagram = Diagram {
        unit: vec![Component {
            chains: vec![
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                Some(CommandBlock {
                    mode: Mode::CommandBlockImpulse,
                    command: "say Hello World!".to_string(),
                    needs_redstone: false,
                    conditional: false,
                    name: "".to_string(),
                    execute_on_first_tick: false,
                    tick_delay: 0,
                    should_track_output: false,
                }),
            ],
            height: 0,
        },
                   Component {
                       chains: vec![
                           None,
                           None,
                           None,
                           None,
                           None,
                           Some(CommandBlock {
                               mode: Mode::CommandBlockImpulse,
                               command: "say Hello World Again!".to_string(),
                               needs_redstone: false,
                               conditional: false,
                               name: "".to_string(),
                               execute_on_first_tick: false,
                               tick_delay: 0,
                               should_track_output: false,
                           }),
                       ],
                       height: 0,
                   }],
    };
    println!("{}", diagram)
}
