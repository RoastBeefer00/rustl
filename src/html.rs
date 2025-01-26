use std::collections::HashMap;

struct Tag {
    name: String,
    attributes: HashMap<String, String>,
    children: Vec<Tag>,
    tag_type: TagType,
}

enum TagType {
    Start,
    End,
    SelfClosing,
}
