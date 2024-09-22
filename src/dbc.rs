#[cfg(feature = "with-serde")]
extern crate serde;
#[cfg(feature = "with-serde")]
#[macro_use]
extern crate serde_derive;

use std::{convert::TryFrom, fs::File, io::prelude::*};

use derive_getters::Getters;

use crate::*;

use nom::{
    branch::{alt, permutation},
    bytes::complete::tag,
    character::complete::{self, line_ending, multispace0, space0, space1},
    combinator::{map, opt},
    multi::{many0, separated_list0},
    sequence::preceded,
    IResult,
};


#[derive(Clone, Debug, PartialEq, Getters)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct DBC {
    /// Version generated by DB editor
    pub(crate) version: Version,
    pub(crate) new_symbols: Vec<Symbol>,
    /// Baud rate of network
    pub(crate) bit_timing: Option<Vec<Baudrate>>,
    /// CAN network nodes
    pub(crate) nodes: Vec<Node>,
    /// Global value table
    pub(crate) value_tables: Vec<ValueTable>,
    /// CAN message (frame) details including signal details
    pub(crate) messages: Vec<Message>,
    pub(crate) message_transmitters: Vec<MessageTransmitter>,
    pub(crate) environment_variables: Vec<EnvironmentVariable>,
    pub(crate) environment_variable_data: Vec<EnvironmentVariableData>,
    pub(crate) signal_types: Vec<SignalType>,
    /// Object comments
    pub(crate) comments: Vec<Comment>,
    pub(crate) attribute_definitions: Vec<AttributeDefinition>,
    // undefined
    // sigtype_attr_list: SigtypeAttrList,
    pub(crate) attribute_defaults: Vec<AttributeDefault>,
    pub(crate) attribute_values: Vec<AttributeValueForObject>,
    /// Encoding for signal raw values
    pub(crate) value_descriptions: Vec<ValueDescription>,
    // obsolete + undefined
    // category_definitions: Vec<CategoryDefinition>,
    // obsolete + undefined
    //categories: Vec<Category>,
    // obsolete + undefined
    //filter: Vec<Filter>,
    pub(crate) signal_type_refs: Vec<SignalTypeRef>,
    /// Signal groups define a group of signals within a message
    pub(crate) signal_groups: Vec<SignalGroups>,
    pub(crate) signal_extended_value_type_list: Vec<SignalExtendedValueTypeList>,
    /// Extended multiplex attributes
    pub(crate) extended_multiplex: Vec<ExtendedMultiplex>,
}

#[allow(dead_code)]
fn dbc_vec_to_string<T: DBCObject>(dbc_objects: &Vec<T>, delimiter: &str) -> String {
    if dbc_objects.len() > 0 {
        dbc_objects
            .into_iter()
            .map(|sym| sym.dbc_string())
            .collect::<Vec<String>>()
            .join(delimiter)
            + "\n"
    } else {
        "".to_string()
    }
}

impl DBCObject for DBC {
    fn dbc_string(&self) -> String {
        let mut file_str = "\n".to_string();
        // Version
        file_str.push_str(&self.version.dbc_string());
        // Symbols
        file_str.push_str("NS_ :\n	");
        file_str.push_str(&dbc_vec_to_string::<Symbol>(&self.new_symbols, "\n	"));

        // Baudrates
        file_str.push_str("BS_:\n");
        match &self.bit_timing {
            Some(bauds) => {
                // confirm delineator
                file_str.push_str(&dbc_vec_to_string::<Baudrate>(&bauds, "\n"));
            }
            None => {}
        }

        // Nodes
        file_str.push_str(&dbc_vec_to_string::<Node>(&self.nodes, " "));

        // Value Tables
        file_str.push_str(&dbc_vec_to_string::<ValueTable>(&self.value_tables, " "));

        // Messages
        file_str.push_str(&dbc_vec_to_string::<Message>(&self.messages, "\n"));
        file_str.push_str(&dbc_vec_to_string::<MessageTransmitter>(&self.message_transmitters, " "));

        // Environment Variables
        file_str.push_str(&dbc_vec_to_string::<EnvironmentVariable>(&self.environment_variables, ""));
        file_str.push_str(&dbc_vec_to_string::<EnvironmentVariableData>(&self.environment_variable_data, ""));

        // Signal Types
        file_str.push_str(&dbc_vec_to_string::<SignalType>(&self.signal_types, " "));

        // Comments
        file_str.push_str(&dbc_vec_to_string::<Comment>(&self.comments, ""));

        // Attributes
        file_str.push_str(&dbc_vec_to_string::<AttributeDefault>(&self.attribute_defaults, ""));
        file_str.push_str(&dbc_vec_to_string::<AttributeDefinition>(&self.attribute_definitions, ""));
        file_str.push_str(&dbc_vec_to_string::<AttributeValueForObject>(&self.attribute_values, ""));

        // Value Descriptions
        file_str.push_str(&dbc_vec_to_string::<ValueDescription>(&self.value_descriptions, ""));

        // Signal Attributes
        file_str.push_str(&dbc_vec_to_string::<SignalTypeRef>(&self.signal_type_refs, ""));
        file_str.push_str(&dbc_vec_to_string::<SignalGroups>(&self.signal_groups, ""));
        file_str.push_str(&dbc_vec_to_string::<SignalExtendedValueTypeList>(
            &self.signal_extended_value_type_list,
            "\n",
        ));

        // Multiplex
        file_str.push_str(&dbc_vec_to_string::<ExtendedMultiplex>(&self.extended_multiplex, ""));

        return file_str;
    }

    fn parse(s: &str) -> IResult<&str, Self>
    where
        Self: Sized,
    {
        let (
            s,
            (
                version,
                new_symbols,
                bit_timing,
                nodes,
                value_tables,
                messages,
                message_transmitters,
                environment_variables,
                environment_variable_data,
                signal_types,
                comments,
                attribute_definitions,
                attribute_defaults,
                attribute_values,
                value_descriptions,
                signal_type_refs,
                signal_groups,
                signal_extended_value_type_list,
                extended_multiplex,
            ),
        ) = permutation((
            Version::parse,
            new_symbols,
            opt(bit_timing),
            many0(Node::parse),
            many0(ValueTable::parse),
            many0(Message::parse),
            many0(MessageTransmitter::parse),
            many0(EnvironmentVariable::parse),
            many0(EnvironmentVariableData::parse),
            many0(SignalType::parse),
            many0(Comment::parse),
            many0(AttributeDefinition::parse),
            many0(AttributeDefault::parse),
            many0(AttributeValueForObject::parse),
            many0(ValueDescription::parse),
            many0(SignalTypeRef::parse),
            many0(SignalGroups::parse),
            many0(SignalExtendedValueTypeList::parse),
            many0(ExtendedMultiplex::parse),
        ))(s)?;
        let (s, _) = multispace0(s)?;
        Ok((
            s,
            DBC {
                version,
                new_symbols,
                bit_timing,
                nodes,
                value_tables,
                messages,
                message_transmitters,
                environment_variables,
                environment_variable_data,
                signal_types,
                comments,
                attribute_definitions,
                attribute_defaults,
                attribute_values,
                value_descriptions,
                signal_type_refs,
                signal_groups,
                signal_extended_value_type_list,
                extended_multiplex,
            },
        ))
    }
}

impl DBC {
    pub fn new() -> Self {
        DBC {
            version: todo!(),
            new_symbols: todo!(),
            bit_timing: todo!(),
            nodes: todo!(),
            value_tables: todo!(),
            messages: todo!(),
            message_transmitters: todo!(),
            environment_variables: todo!(),
            environment_variable_data: todo!(),
            signal_types: todo!(),
            comments: todo!(),
            attribute_definitions: todo!(),
            attribute_defaults: todo!(),
            attribute_values: todo!(),
            value_descriptions: todo!(),
            signal_type_refs: todo!(),
            signal_groups: todo!(),
            signal_extended_value_type_list: todo!(),
            extended_multiplex: todo!(),
        }
    }

    pub fn write_to_file(&self, file: &'static str) -> std::io::Result<()> {
        let mut file = File::create(file)?;
        file.write_all(&self.dbc_string().as_bytes())?;
        Ok(())
    }

    pub fn read_from_file(file: &'static str) -> std::io::Result<Self> {
        let mut f = File::open(file)?;
        let mut buffer = Vec::new();
        f.read_to_end(&mut buffer)?;

        Ok(Self::from_slice(&buffer).expect("Failed to parse dbc file"))
    }

    /// Read a DBC from a buffer
    #[allow(clippy::result_large_err)]
    pub fn from_slice(buffer: &[u8]) -> Result<DBC, Error> {
        let dbc_in = std::str::from_utf8(buffer).unwrap();
        Self::try_from(dbc_in)
    }

    #[allow(clippy::should_implement_trait)]
    #[deprecated(since = "4.0.0", note = "please use `DBC::try_from` instead")]
    #[allow(clippy::result_large_err)]
    pub fn from_str(dbc_in: &str) -> Result<DBC, Error> {
        let (remaining, dbc) = DBC::parse(dbc_in).map_err(Error::Nom)?;
        if !remaining.is_empty() {
            return Err(Error::Incomplete(dbc, remaining));
        }
        Ok(dbc)
    }

    pub fn signal_by_name(&self, message_id: MessageId, signal_name: &str) -> Option<&Signal> {
        let message = self
            .messages
            .iter()
            .find(|message| message.message_id == message_id);

        if let Some(message) = message {
            return message
                .signals
                .iter()
                .find(|signal| signal.name == *signal_name);
        }
        None
    }

    /// Lookup a message comment
    pub fn message_comment(&self, message_id: MessageId) -> Option<&str> {
        self.comments
            .iter()
            .filter_map(|x| match x {
                Comment::Message {
                    message_id: ref x_message_id,
                    ref comment,
                } => {
                    if *x_message_id == message_id {
                        Some(comment.as_str())
                    } else {
                        None
                    }
                }
                _ => None,
            })
            .next()
    }

    /// Lookup a signal comment
    pub fn signal_comment(&self, message_id: MessageId, signal_name: &str) -> Option<&str> {
        self.comments
            .iter()
            .filter_map(|x| match x {
                Comment::Signal {
                    message_id: ref x_message_id,
                    signal_name: ref x_signal_name,
                    comment,
                } => {
                    if *x_message_id == message_id && x_signal_name == signal_name {
                        Some(comment.as_str())
                    } else {
                        None
                    }
                }
                _ => None,
            })
            .next()
    }

    /// Lookup value descriptions for signal
    pub fn value_descriptions_for_signal(
        &self,
        message_id: MessageId,
        signal_name: &str,
    ) -> Option<&[ValDescription]> {
        self.value_descriptions
            .iter()
            .filter_map(|x| match x {
                ValueDescription::Signal {
                    message_id: ref x_message_id,
                    signal_name: ref x_signal_name,
                    ref value_descriptions,
                } => {
                    if *x_message_id == message_id && x_signal_name == signal_name {
                        Some(value_descriptions.as_slice())
                    } else {
                        None
                    }
                }
                _ => None,
            })
            .next()
    }

    /// Lookup the extended value for a given signal
    pub fn extended_value_type_for_signal(
        &self,
        message_id: MessageId,
        signal_name: &str,
    ) -> Option<&SignalExtendedValueType> {
        self.signal_extended_value_type_list
            .iter()
            .filter_map(|x| {
                let SignalExtendedValueTypeList {
                    message_id: ref x_message_id,
                    signal_name: ref x_signal_name,
                    ref signal_extended_value_type,
                } = x;
                if *x_message_id == message_id && x_signal_name == signal_name {
                    Some(signal_extended_value_type)
                } else {
                    None
                }
            })
            .next()
    }

    /// Lookup the message multiplexor switch signal for a given message
    /// This does not work for extended multiplexed messages, if multiple multiplexors are defined for a message a Error is returned.
    #[allow(clippy::result_large_err)]
    pub fn message_multiplexor_switch(
        &self,
        message_id: MessageId,
    ) -> Result<Option<&Signal>, Error> {
        let message = self
            .messages
            .iter()
            .find(|message| message.message_id == message_id);

        if let Some(message) = message {
            if self
                .extended_multiplex
                .iter()
                .any(|ext_mp| ext_mp.message_id == message_id)
            {
                Err(Error::MultipleMultiplexors)
            } else {
                Ok(message
                    .signals
                    .iter()
                    .find(|signal| signal.multiplexer_indicator == MultiplexIndicator::Multiplexor))
            }
        } else {
            Ok(None)
        }
    }
}

impl<'a> TryFrom<&'a str> for DBC {
    type Error = Error<'a>;

    fn try_from(dbc_in: &'a str) -> Result<Self, Self::Error> {
        let (remaining, dbc) = DBC::parse(dbc_in).map_err(Error::Nom)?;
        if !remaining.is_empty() {
            return Err(Error::Incomplete(dbc, remaining));
        }
        Ok(dbc)
    }
}

/// Baudrate of network in kbit/s
#[derive(Copy, Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Baudrate(pub(crate) u64);

impl DBCObject for Baudrate {
    fn dbc_string(&self) -> String {
        return self.0.to_string();
    }

    fn parse(s: &str) -> nom::IResult<&str, Self> {
        let (s, baudrate) = map(complete::u64, Baudrate)(s)?;
        Ok((s, baudrate))
    }
}

fn bit_timing(s: &str) -> IResult<&str, Vec<Baudrate>> {
    let (s, _) = multispace0(s)?;
    let (s, _) = tag("BS_:")(s)?;
    let (s, baudrates) = opt(preceded(
        parser::ms1,
        separated_list0(parser::comma, map(complete::u64, Baudrate)),
    ))(s)?;
    Ok((s, baudrates.unwrap_or_default()))
}

/// Version generated by DB editor
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Version(pub String);

impl DBCObject for Version {
    fn dbc_string(&self) -> String {
        return format!("VERSION \"{}\"\n", self.0);
    }

    fn parse(s: &str) -> IResult<&str, Self>
    where
        Self: Sized,
    {
        let (s, _) = multispace0(s)?;
        let (s, _) = tag("VERSION")(s)?;
        let (s, _) = parser::ms1(s)?;
        let (s, v) = parser::char_string(s)?;
        let (s, _) = line_ending(s)?;
        Ok((s, Version(v.to_string())))
    }
}

#[test]
fn version_test() {
    let def = "VERSION \"HNPBNNNYNNNNNNNNNNNNNNNNNNNNNNNNYNYYYYYYYY>4>%%%/4>'%**4YYY///\"\n";
    let version_exp =
        Version("HNPBNNNYNNNNNNNNNNNNNNNNNNNNNNNNYNYYYYYYYY>4>%%%/4>'%**4YYY///".to_string());
    let (_, version) = Version::parse(def).unwrap();

    // Test parsing
    assert_eq!(version_exp, version);

    // Test generation
    assert_eq!(def, version.dbc_string());
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Symbol(pub String);

impl DBCObject for Symbol {
    fn dbc_string(&self) -> String {
        return self.0.to_string();
    }

    fn parse(s: &str) -> IResult<&str, Self>
    where
        Self: Sized,
    {
        let (s, _) = space1(s)?;
        let (s, symbol) = parser::c_ident(s)?;
        let (s, _) = line_ending(s)?;
        Ok((s, Symbol(symbol)))
    }
}

#[test]
fn new_symbols_test() {
    let def = "NS_ :
            NS_DESC_
            CM_
            BA_DEF_

        ";
    let symbols_exp = vec![
        Symbol("NS_DESC_".to_string()),
        Symbol("CM_".to_string()),
        Symbol("BA_DEF_".to_string()),
    ];
    let (_, symbols) = new_symbols(def).unwrap();

    // Test parsing
    assert_eq!(symbols_exp, symbols);

    // Test generation
    // assert_eq!(def, symbols.dbc_string());
}

pub(crate) fn new_symbols(s: &str) -> IResult<&str, Vec<Symbol>> {
    let (s, _) = multispace0(s)?;
    let (s, _) = tag("NS_ :")(s)?;
    let (s, _) = space0(s)?;
    let (s, _) = line_ending(s)?;
    let (s, symbols) = many0(Symbol::parse)(s)?;
    Ok((s, symbols))
}

/// Object comments
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum Comment {
    Node {
        node_name: String,
        comment: String,
    },
    Message {
        message_id: MessageId,
        comment: String,
    },
    Signal {
        message_id: MessageId,
        signal_name: String,
        comment: String,
    },
    EnvVar {
        env_var_name: String,
        comment: String,
    },
    Plain {
        comment: String,
    },
}

impl Comment {
    fn node_comment(s: &str) -> IResult<&str, Comment> {
        let (s, _) = tag("BU_")(s)?;
        let (s, _) = parser::ms1(s)?;
        let (s, node_name) = parser::c_ident(s)?;
        let (s, _) = parser::ms1(s)?;
        let (s, comment) = parser::char_string(s)?;

        Ok((
            s,
            Comment::Node {
                node_name,
                comment: comment.to_string(),
            },
        ))
    }

    fn message_comment(s: &str) -> IResult<&str, Comment> {
        let (s, _) = tag("BO_")(s)?;
        let (s, _) = parser::ms1(s)?;
        let (s, message_id) = MessageId::parse(s)?;
        let (s, _) = parser::ms1(s)?;
        let (s, comment) = parser::char_string(s)?;

        Ok((
            s,
            Comment::Message {
                message_id,
                comment: comment.to_string(),
            },
        ))
    }

    fn signal_comment(s: &str) -> IResult<&str, Comment> {
        let (s, _) = tag("SG_")(s)?;
        let (s, _) = parser::ms1(s)?;
        let (s, message_id) = MessageId::parse(s)?;
        let (s, _) = parser::ms1(s)?;
        let (s, signal_name) = parser::c_ident(s)?;
        let (s, _) = parser::ms1(s)?;
        let (s, comment) = parser::char_string(s)?;
        Ok((
            s,
            Comment::Signal {
                message_id,
                signal_name,
                comment: comment.to_string(),
            },
        ))
    }

    fn env_var_comment(s: &str) -> IResult<&str, Comment> {
        let (s, _) = parser::ms0(s)?;
        let (s, _) = tag("EV_")(s)?;
        let (s, _) = parser::ms1(s)?;
        let (s, env_var_name) = parser::c_ident(s)?;
        let (s, _) = parser::ms1(s)?;
        let (s, comment) = parser::char_string(s)?;
        Ok((
            s,
            Comment::EnvVar {
                env_var_name,
                comment: comment.to_string(),
            },
        ))
    }

    fn comment_plain(s: &str) -> IResult<&str, Comment> {
        let (s, comment) = parser::char_string(s)?;
        Ok((
            s,
            Comment::Plain {
                comment: comment.to_string(),
            },
        ))
    }
}

impl DBCObject for Comment {
    fn dbc_string(&self) -> String {
        return match self {
            Self::Node { node_name, comment } => {
                format!("CM_ BU_ {} \"{}\";\n", node_name, comment)
            }
            Self::Message {
                message_id,
                comment,
            } => {
                format!("CM_ BO_ {} \"{}\";\n", message_id.dbc_string(), comment,)
            }
            Self::Signal {
                message_id,
                signal_name,
                comment,
            } => {
                format!(
                    "CM_ SG_ {} {} \"{}\";\n",
                    message_id.dbc_string(),
                    signal_name,
                    comment
                )
            }
            Self::EnvVar {
                env_var_name,
                comment,
            } => {
                format!("CM_ EV_ {} \"{}\";\n", env_var_name, comment,)
            }
            Self::Plain { comment } => format!("\"{}\"", comment),
        };
    }

    fn parse(s: &str) -> IResult<&str, Self>
    where
        Self: Sized,
    {
        let (s, _) = multispace0(s)?;
        let (s, _) = tag("CM_")(s)?;
        let (s, _) = parser::ms1(s)?;
        let (s, comment) = alt((
            Self::node_comment,
            Self::message_comment,
            Self::env_var_comment,
            Self::signal_comment,
            Self::comment_plain,
        ))(s)?;
        let (s, _) = parser::semi_colon(s)?;
        let (s, _) = line_ending(s)?;
        Ok((s, comment))
    }
}

#[test]
fn signal_comment_test() {
    let def1 = "CM_ SG_ 193 KLU_R_X \"This is a signal comment test\";\n";
    let message_id = MessageId::Standard(193);
    let comment1 = Comment::Signal {
        message_id,
        signal_name: "KLU_R_X".to_string(),
        comment: "This is a signal comment test".to_string(),
    };
    let (_, comment1_def) =
        Comment::parse(def1).expect("Failed to parse signal comment definition");

    // Test parsing
    assert_eq!(comment1, comment1_def);

    // Test generation
    assert_eq!(def1, comment1.dbc_string());
}

#[test]
fn message_definition_comment_test() {
    let def1 = "CM_ BO_ 34544 \"Some Message comment\";\n";
    let message_id = MessageId::Standard(34544);
    let comment1 = Comment::Message {
        message_id,
        comment: "Some Message comment".to_string(),
    };
    let (_, comment1_def) =
        Comment::parse(def1).expect("Failed to parse message definition comment definition");

    // Test parsing
    assert_eq!(comment1, comment1_def);

    // Test generation
    assert_eq!(def1, comment1.dbc_string());
}

#[test]
fn node_comment_test() {
    let def1 = "CM_ BU_ network_node \"Some network node comment\";\n";
    let comment1 = Comment::Node {
        node_name: "network_node".to_string(),
        comment: "Some network node comment".to_string(),
    };
    let (_, comment1_def) = Comment::parse(def1).expect("Failed to parse node comment definition");

    // Test parsing
    assert_eq!(comment1, comment1_def);

    // Test generation
    assert_eq!(def1, comment1.dbc_string());
}

#[test]
fn env_var_comment_test() {
    let def1 = "CM_ EV_ ENVXYZ \"Some env var name comment\";\n";
    let comment1 = Comment::EnvVar {
        env_var_name: "ENVXYZ".to_string(),
        comment: "Some env var name comment".to_string(),
    };
    let (_, comment1_def) =
        Comment::parse(def1).expect("Failed to parse env var comment definition");

    // Test parsing
    assert_eq!(comment1, comment1_def);

    // Test generation
    assert_eq!(def1, comment1.dbc_string());
}
