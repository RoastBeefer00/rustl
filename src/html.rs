use crate::node::Node;
use std::collections::HashMap;

pub(crate) struct Tag {
    pub name: String,
    pub attributes: HashMap<String, String>,
    pub children: Vec<Node>,
    pub tag_type: TagType,
}

pub(crate) enum TagType {
    Start,
    End,
    SelfClosing,
}
