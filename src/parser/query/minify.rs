use super::ast::*;
use crate::parser::tokenizer::{Kind, Token, TokenStream};
use combine::StreamOnce;
use thiserror::Error;

/// Error minifying query
#[derive(Error, Debug)]
#[error("query minify error: {}", _0)]
pub struct MinifyError(String);

pub fn minify_query(source: &str) -> Result<String, MinifyError> {
    let mut bits: Vec<&str> = Vec::new();
    let mut stream = TokenStream::new(source);
    let mut prev_was_punctuator = false;

    loop {
        match stream.uncons() {
            Ok(x) => {
                let token: Token = x;
                let is_non_punctuator = token.kind != Kind::Punctuator;

                if prev_was_punctuator && is_non_punctuator {
                    bits.push(" ");
                }

                bits.push(token.value);
                prev_was_punctuator = is_non_punctuator;
            }
            Err(ref e) if e == &combine::easy::Error::end_of_input() => break,
            Err(e) => return Err(MinifyError(e.to_string())),
        }
    }

    Ok(bits.join(""))
}

/// Minify a document according to the same rules as `minify_query`
pub fn minify_document<'a, T: Text<'a>>(doc: &Document<'a, T>) -> String {
    let mut minifier = Minifier::new();
    minifier.write_document(doc);
    minifier.buf
}

struct Minifier {
    buf: String,
    last_was_non_punctuator: bool,
}

impl Minifier {
    fn new() -> Self {
        Self {
            buf: String::with_capacity(1024),
            last_was_non_punctuator: false,
        }
    }

    fn write_non_punctuator(&mut self, s: &str) {
        if self.last_was_non_punctuator {
            self.buf.push(' ');
        }
        self.buf.push_str(s);
        self.last_was_non_punctuator = true;
    }

    fn write_punctuator(&mut self, s: &str) {
        self.buf.push_str(s);
        self.last_was_non_punctuator = false;
    }

    fn write_document<'a, T: Text<'a>>(&mut self, doc: &Document<'a, T>) {
        for def in &doc.definitions {
            self.write_definition(def);
        }
    }

    fn write_definition<'a, T: Text<'a>>(&mut self, def: &Definition<'a, T>) {
        match def {
            Definition::Operation(op) => self.write_operation(op),
            Definition::Fragment(frag) => self.write_fragment(frag),
        }
    }

    fn write_operation<'a, T: Text<'a>>(&mut self, op: &OperationDefinition<'a, T>) {
        match op {
            OperationDefinition::SelectionSet(set) => self.write_selection_set(set),
            OperationDefinition::Query(q) => {
                self.write_non_punctuator("query");
                if let Some(ref name) = q.name {
                    self.write_non_punctuator(name.as_ref());
                }
                self.write_variable_definitions(&q.variable_definitions);
                self.write_directives(&q.directives);
                self.write_selection_set(&q.selection_set);
            }
            OperationDefinition::Mutation(m) => {
                self.write_non_punctuator("mutation");
                if let Some(ref name) = m.name {
                    self.write_non_punctuator(name.as_ref());
                }
                self.write_variable_definitions(&m.variable_definitions);
                self.write_directives(&m.directives);
                self.write_selection_set(&m.selection_set);
            }
            OperationDefinition::Subscription(s) => {
                self.write_non_punctuator("subscription");
                if let Some(ref name) = s.name {
                    self.write_non_punctuator(name.as_ref());
                }
                self.write_variable_definitions(&s.variable_definitions);
                self.write_directives(&s.directives);
                self.write_selection_set(&s.selection_set);
            }
        }
    }

    fn write_fragment<'a, T: Text<'a>>(&mut self, frag: &FragmentDefinition<'a, T>) {
        self.write_non_punctuator("fragment");
        self.write_non_punctuator(frag.name.as_ref());
        self.write_type_condition(&frag.type_condition);
        self.write_directives(&frag.directives);
        self.write_selection_set(&frag.selection_set);
    }

    fn write_selection_set<'a, T: Text<'a>>(&mut self, set: &SelectionSet<'a, T>) {
        self.write_punctuator("{");
        for item in &set.items {
            self.write_selection(item);
        }
        self.write_punctuator("}");
    }

    fn write_selection<'a, T: Text<'a>>(&mut self, selection: &Selection<'a, T>) {
        match selection {
            Selection::Field(f) => {
                if let Some(ref alias) = f.alias {
                    self.write_non_punctuator(alias.as_ref());
                    self.write_punctuator(":");
                }
                self.write_non_punctuator(f.name.as_ref());
                self.write_arguments(&f.arguments);
                self.write_directives(&f.directives);
                if !f.selection_set.items.is_empty() {
                    self.write_selection_set(&f.selection_set);
                }
            }
            Selection::FragmentSpread(fs) => {
                self.write_punctuator("...");
                self.write_non_punctuator(fs.fragment_name.as_ref());
                self.write_directives(&fs.directives);
            }
            Selection::InlineFragment(ifrag) => {
                self.write_punctuator("...");
                if let Some(ref tc) = ifrag.type_condition {
                    self.write_type_condition(tc);
                }
                self.write_directives(&ifrag.directives);
                self.write_selection_set(&ifrag.selection_set);
            }
        }
    }

    fn write_type_condition<'a, T: Text<'a>>(&mut self, tc: &TypeCondition<'a, T>) {
        match tc {
            TypeCondition::On(name) => {
                self.write_non_punctuator("on");
                self.write_non_punctuator(name.as_ref());
            }
        }
    }

    fn write_variable_definitions<'a, T: Text<'a>>(&mut self, vars: &[VariableDefinition<'a, T>]) {
        if vars.is_empty() {
            return;
        }
        self.write_punctuator("(");
        for var in vars {
            self.write_punctuator("$");
            self.write_non_punctuator(var.name.as_ref());
            self.write_punctuator(":");
            self.write_type(&var.var_type);
            if let Some(ref def) = var.default_value {
                self.write_punctuator("=");
                self.write_value(def);
            }
        }
        self.write_punctuator(")");
    }

    fn write_type<'a, T: Text<'a>>(&mut self, ty: &Type<'a, T>) {
        match ty {
            Type::NamedType(name) => self.write_non_punctuator(name.as_ref()),
            Type::ListType(inner) => {
                self.write_punctuator("[");
                self.write_type(inner);
                self.write_punctuator("]");
            }
            Type::NonNullType(inner) => {
                self.write_type(inner);
                self.write_punctuator("!");
            }
        }
    }

    fn write_directives<'a, T: Text<'a>>(&mut self, dirs: &[Directive<'a, T>]) {
        for dir in dirs {
            self.write_punctuator("@");
            self.write_non_punctuator(dir.name.as_ref());
            self.write_arguments(&dir.arguments);
        }
    }

    fn write_arguments<'a, T: Text<'a>>(&mut self, args: &[(T::Value, Value<'a, T>)]) {
        if args.is_empty() {
            return;
        }
        self.write_punctuator("(");
        for (name, val) in args {
            self.write_non_punctuator(name.as_ref());
            self.write_punctuator(":");
            self.write_value(val);
        }
        self.write_punctuator(")");
    }

    fn write_value<'a, T: Text<'a>>(&mut self, val: &Value<'a, T>) {
        match val {
            Value::Variable(name) => {
                self.write_punctuator("$");
                self.write_non_punctuator(name.as_ref());
            }
            Value::Int(n) => self.write_non_punctuator(&n.0.to_string()),
            Value::Float(f) => self.write_non_punctuator(&f.to_string()),
            Value::String(s) => {
                if self.last_was_non_punctuator {
                    self.buf.push(' ');
                }
                self.buf.push('"');
                for c in s.chars() {
                    match c {
                        '\x08' => self.buf.push_str("\\b"),
                        '\x0c' => self.buf.push_str("\\f"),
                        '\r' => self.buf.push_str("\\r"),
                        '\n' => self.buf.push_str("\\n"),
                        '\t' => self.buf.push_str("\\t"),
                        '"' => self.buf.push_str("\\\""),
                        '\\' => self.buf.push_str("\\\\"),
                        c if c.is_control() => {
                            use std::fmt::Write;
                            write!(&mut self.buf, "\\u{:04x}", c as u32).unwrap();
                        }
                        c => self.buf.push(c),
                    }
                }
                self.buf.push('"');
                self.last_was_non_punctuator = true;
            }
            Value::Boolean(b) => self.write_non_punctuator(if *b { "true" } else { "false" }),
            Value::Null => self.write_non_punctuator("null"),
            Value::Enum(name) => self.write_non_punctuator(name.as_ref()),
            Value::List(items) => {
                self.write_punctuator("[");
                for item in items {
                    self.write_value(item);
                }
                self.write_punctuator("]");
            }
            Value::Object(fields) => {
                self.write_punctuator("{");
                for (name, val) in fields {
                    self.write_non_punctuator(name.as_ref());
                    self.write_punctuator(":");
                    self.write_value(val);
                }
                self.write_punctuator("}");
            }
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn strip_ignored_characters() {
        let source = "
        query SomeQuery($foo: String!, $bar: String) {
            someField(foo: $foo, bar: $bar) {
                a
                b {
                    ... on B {
                        c
                        d
                    }
                }
            }
        }
        ";

        let minified =
            super::minify_query(source.to_string().as_str()).expect("minification failed");

        assert_eq!(
            &minified,
            "query SomeQuery($foo:String!$bar:String){someField(foo:$foo bar:$bar){a b{...on B{c d}}}}"
        );
    }

    #[test]
    fn unexpected_token() {
        let source = "
        query foo {
            bar;
        }
        ";

        let minified = super::minify_query(source.to_string().as_str());

        assert!(minified.is_err());

        assert_eq!(
            minified.unwrap_err().to_string(),
            "query minify error: Unexpected unexpected character ';'"
        );
    }

    #[test]
    fn minify_document_test() {
        let source = "
        query SomeQuery($foo: String!, $bar: String) {
            someField(foo: $foo, bar: $bar) {
                a
                b {
                    ... on B {
                        c
                        d
                    }
                }
            }
        }
        ";

        let doc =
            crate::parser::query::grammar::parse_query::<String>(source).expect("parse failed");
        let minified_doc = super::minify_document(&doc);
        let minified_query = super::minify_query(source).expect("minification failed");

        assert_eq!(minified_doc, minified_query);
    }

    #[test]
    fn minify_document_complex() {
        let source = r#"
        mutation DoSomething($input: UpdateInput! = { a: 1, b: "foo" }) @opt(level: 1) {
            updateItem(id: "123", data: $input) {
                id
                ... on Item {
                    name
                    tags
                }
                ...FragmentName
            }
        }
        fragment FragmentName on Item {
            owner {
                id
                email
            }
        }
        "#;

        let doc =
            crate::parser::query::grammar::parse_query::<String>(source).expect("parse failed");
        let minified_doc = super::minify_document(&doc);
        let minified_query = super::minify_query(source).expect("minification failed");

        assert_eq!(minified_doc, minified_query);
    }

    // #[test]
    // fn minify_all_queries() {
    //     use std::fs;
    //     let paths = fs::read_dir("src/parser/tests/queries").unwrap();
    //     let mut failed = false;

    //     for path in paths {
    //         let path = path.unwrap().path();
    //         if path.extension().and_then(|s| s.to_str()) == Some("graphql") {
    //             let source = fs::read_to_string(&path).unwrap();
    //             let doc = crate::parser::query::grammar::parse_query::<String>(&source)
    //                 .expect(&format!("parse failed for {:?}", path));
    //             let minified_doc = super::minify_document(&doc);
    //             let minified_query = super::minify_query(&source)
    //                 .expect(&format!("minification failed for {:?}", path));

    //             if minified_doc != minified_query {
    //                 failed = true;
    //                 println!("Minification failed for {:?}", path);
    //                 println!("Minified Document: \"{}\"", minified_doc);
    //                 println!("Minified Query:    \"{}\"", minified_query);
    //             }
    //         }
    //     }

    //     if failed {
    //         panic!("Some queries failed to minify correctly");
    //     }
    // }

    #[test]
    fn test_minify_directive_args() {
        let source =
            std::fs::read_to_string("src/parser/tests/queries/directive_args.graphql").unwrap();
        let minified_query = super::minify_query(&source).unwrap();
        insta::assert_snapshot!(minified_query, @r#"query{node@dir(a:1 b:"2" c:true d:false e:null)}"#);
    }

    #[test]
    fn test_minify_directive_args_multiline() {
        let source =
            std::fs::read_to_string("src/parser/tests/queries/directive_args_multiline.graphql")
                .unwrap();
        let minified_query = super::minify_query(&source).unwrap();
        insta::assert_snapshot!(minified_query, @r#"query{node@dir(a:1 b:"2" c:true d:false e:null)}"#);
    }

    #[test]
    fn test_minify_fragment() {
        let source = std::fs::read_to_string("src/parser/tests/queries/fragment.graphql").unwrap();
        let minified_query = super::minify_query(&source).unwrap();
        insta::assert_snapshot!(minified_query, @"fragment frag on Friend{node}");
    }

    #[test]
    fn test_minify_fragment_spread() {
        let source =
            std::fs::read_to_string("src/parser/tests/queries/fragment_spread.graphql").unwrap();
        let minified_query = super::minify_query(&source).unwrap();
        insta::assert_snapshot!(minified_query, @"query{node{id...something}}");
    }

    #[test]
    fn test_minify_inline_fragment() {
        let source =
            std::fs::read_to_string("src/parser/tests/queries/inline_fragment.graphql").unwrap();
        let minified_query = super::minify_query(&source).unwrap();
        insta::assert_snapshot!(minified_query, @"query{node{id...on User{name}}}");
    }

    #[test]
    fn test_minify_inline_fragment_dir() {
        let source =
            std::fs::read_to_string("src/parser/tests/queries/inline_fragment_dir.graphql")
                .unwrap();
        let minified_query = super::minify_query(&source).unwrap();
        insta::assert_snapshot!(minified_query, @"query{node{id...on User@defer{name}}}");
    }

    #[test]
    fn test_minify_kitchen_sink() {
        let source =
            std::fs::read_to_string("src/parser/tests/queries/kitchen-sink.graphql").unwrap();
        let minified_query = super::minify_query(&source).unwrap();
        insta::assert_snapshot!(minified_query, @r#"
        query queryName($foo:ComplexType$site:Site=MOBILE){whoever123is:node(id:[123 456]){id...on User@defer{field2{id alias:field1(first:10 after:$foo)@include(if:$foo){id...frag}}}...@skip(unless:$foo){id}...{id}}}mutation likeStory{like(story:123)@defer{story{id}}}subscription StoryLikeSubscription($input:StoryLikeSubscribeInput){storyLikeSubscribe(input:$input){story{likers{count}likeSentence{text}}}}fragment frag on Friend{foo(size:$size bar:$b obj:{key:"value" block:"""

              block string uses \"""

          """})}{unnamed(truthy:true falsey:false nullish:null)query}
        "#);
    }

    #[test]
    fn test_minify_kitchen_sink_canonical() {
        let source =
            std::fs::read_to_string("src/parser/tests/queries/kitchen-sink_canonical.graphql")
                .unwrap();
        let minified_query = super::minify_query(&source).unwrap();
        insta::assert_snapshot!(minified_query, @r#"query queryName($foo:ComplexType$site:Site=MOBILE){whoever123is:node(id:[123 456]){id...on User@defer{field2{id alias:field1(first:10 after:$foo)@include(if:$foo){id...frag}}}...@skip(unless:$foo){id}...{id}}}mutation likeStory{like(story:123)@defer{story{id}}}subscription StoryLikeSubscription($input:StoryLikeSubscribeInput){storyLikeSubscribe(input:$input){story{likers{count}likeSentence{text}}}}fragment frag on Friend{foo(size:$size bar:$b obj:{block:"block string uses \"\"\"" key:"value"})}{unnamed(truthy:true falsey:false nullish:null)query}"#);
    }

    #[test]
    fn test_minify_minimal() {
        let source = std::fs::read_to_string("src/parser/tests/queries/minimal.graphql").unwrap();
        let minified_query = super::minify_query(&source).unwrap();
        insta::assert_snapshot!(minified_query, @"{a}");
    }

    #[test]
    fn test_minify_minimal_mutation() {
        let source =
            std::fs::read_to_string("src/parser/tests/queries/minimal_mutation.graphql").unwrap();
        let minified_query = super::minify_query(&source).unwrap();
        insta::assert_snapshot!(minified_query, @"mutation{notify}");
    }

    #[test]
    fn test_minify_minimal_query() {
        let source =
            std::fs::read_to_string("src/parser/tests/queries/minimal_query.graphql").unwrap();
        let minified_query = super::minify_query(&source).unwrap();
        insta::assert_snapshot!(minified_query, @"query{node}");
    }

    #[test]
    fn test_minify_mutation_directive() {
        let source =
            std::fs::read_to_string("src/parser/tests/queries/mutation_directive.graphql").unwrap();
        let minified_query = super::minify_query(&source).unwrap();
        insta::assert_snapshot!(minified_query, @"mutation@directive{node}");
    }

    #[test]
    fn test_minify_mutation_nameless_vars() {
        let source =
            std::fs::read_to_string("src/parser/tests/queries/mutation_nameless_vars.graphql")
                .unwrap();
        let minified_query = super::minify_query(&source).unwrap();
        insta::assert_snapshot!(minified_query, @"mutation($first:Int$second:Int){field1(first:$first)field2(second:$second)}");
    }

    #[test]
    fn test_minify_named_query() {
        let source =
            std::fs::read_to_string("src/parser/tests/queries/named_query.graphql").unwrap();
        let minified_query = super::minify_query(&source).unwrap();
        insta::assert_snapshot!(minified_query, @"query Foo{field}");
    }

    #[test]
    fn test_minify_nested_selection() {
        let source =
            std::fs::read_to_string("src/parser/tests/queries/nested_selection.graphql").unwrap();
        let minified_query = super::minify_query(&source).unwrap();
        insta::assert_snapshot!(minified_query, @"query{node{id}}");
    }

    #[test]
    fn test_minify_query_aliases() {
        let source =
            std::fs::read_to_string("src/parser/tests/queries/query_aliases.graphql").unwrap();
        let minified_query = super::minify_query(&source).unwrap();
        insta::assert_snapshot!(minified_query, @"query{an_alias:node}");
    }

    #[test]
    fn test_minify_query_arguments() {
        let source =
            std::fs::read_to_string("src/parser/tests/queries/query_arguments.graphql").unwrap();
        let minified_query = super::minify_query(&source).unwrap();
        insta::assert_snapshot!(minified_query, @"query{node(id:1)}");
    }

    #[test]
    fn test_minify_query_arguments_multiline() {
        let source =
            std::fs::read_to_string("src/parser/tests/queries/query_arguments_multiline.graphql")
                .unwrap();
        let minified_query = super::minify_query(&source).unwrap();
        insta::assert_snapshot!(minified_query, @"query{node(id:1)node(id:1 one:3)}");
    }

    #[test]
    fn test_minify_query_array_argument_multiline() {
        let source = std::fs::read_to_string(
            "src/parser/tests/queries/query_array_argument_multiline.graphql",
        )
        .unwrap();
        let minified_query = super::minify_query(&source).unwrap();
        insta::assert_snapshot!(minified_query, @"query{node(id:[5 6 7])}");
    }

    #[test]
    fn test_minify_query_directive() {
        let source =
            std::fs::read_to_string("src/parser/tests/queries/query_directive.graphql").unwrap();
        let minified_query = super::minify_query(&source).unwrap();
        insta::assert_snapshot!(minified_query, @"query@directive{node}");
    }

    #[test]
    fn test_minify_query_list_argument() {
        let source =
            std::fs::read_to_string("src/parser/tests/queries/query_list_argument.graphql")
                .unwrap();
        let minified_query = super::minify_query(&source).unwrap();
        insta::assert_snapshot!(minified_query, @"query{node(id:1 list:[123 456])}");
    }

    #[test]
    fn test_minify_query_nameless_vars() {
        let source =
            std::fs::read_to_string("src/parser/tests/queries/query_nameless_vars.graphql")
                .unwrap();
        let minified_query = super::minify_query(&source).unwrap();
        insta::assert_snapshot!(minified_query, @"query($first:Int$second:Int){field1(first:$first)field2(second:$second)}");
    }

    #[test]
    fn test_minify_query_nameless_vars_multiple_fields() {
        let source = std::fs::read_to_string(
            "src/parser/tests/queries/query_nameless_vars_multiple_fields.graphql",
        )
        .unwrap();
        let minified_query = super::minify_query(&source).unwrap();
        insta::assert_snapshot!(minified_query, @"query($houseId:String!$streetNumber:Int!){house(id:$houseId){id name lat lng}street(number:$streetNumber){id}houseStreet(id:$houseId number:$streetNumber){id}}");
    }

    #[test]
    fn test_minify_query_nameless_vars_multiple_fields_canonical() {
        let source = std::fs::read_to_string(
            "src/parser/tests/queries/query_nameless_vars_multiple_fields_canonical.graphql",
        )
        .unwrap();
        let minified_query = super::minify_query(&source).unwrap();
        insta::assert_snapshot!(minified_query, @"query($houseId:String!$streetNumber:Int!){house(id:$houseId){id name lat lng}street(number:$streetNumber){id}houseStreet(id:$houseId number:$streetNumber){id}}");
    }

    #[test]
    fn test_minify_query_object_argument() {
        let source =
            std::fs::read_to_string("src/parser/tests/queries/query_object_argument.graphql")
                .unwrap();
        let minified_query = super::minify_query(&source).unwrap();
        insta::assert_snapshot!(minified_query, @"query{node(id:1 obj:{key1:123 key2:456})}");
    }

    #[test]
    fn test_minify_query_object_argument_multiline() {
        let source = std::fs::read_to_string(
            "src/parser/tests/queries/query_object_argument_multiline.graphql",
        )
        .unwrap();
        let minified_query = super::minify_query(&source).unwrap();
        insta::assert_snapshot!(minified_query, @"query{node(id:1 obj:{key1:123 key2:456})}");
    }

    #[test]
    fn test_minify_query_var_default_float() {
        let source =
            std::fs::read_to_string("src/parser/tests/queries/query_var_default_float.graphql")
                .unwrap();
        let minified_query = super::minify_query(&source).unwrap();
        insta::assert_snapshot!(minified_query, @"query Foo($site:Float=0.5){field}");
    }

    #[test]
    fn test_minify_query_var_default_list() {
        let source =
            std::fs::read_to_string("src/parser/tests/queries/query_var_default_list.graphql")
                .unwrap();
        let minified_query = super::minify_query(&source).unwrap();
        insta::assert_snapshot!(minified_query, @"query Foo($site:[Int]=[123 456]){field}");
    }

    #[test]
    fn test_minify_query_var_default_object() {
        let source =
            std::fs::read_to_string("src/parser/tests/queries/query_var_default_object.graphql")
                .unwrap();
        let minified_query = super::minify_query(&source).unwrap();
        insta::assert_snapshot!(minified_query, @"query Foo($site:Site={url:null}){field}");
    }

    #[test]
    fn test_minify_query_var_default_string() {
        let source =
            std::fs::read_to_string("src/parser/tests/queries/query_var_default_string.graphql")
                .unwrap();
        let minified_query = super::minify_query(&source).unwrap();
        insta::assert_snapshot!(minified_query, @r#"query Foo($site:String="string"){field}"#);
    }

    #[test]
    fn test_minify_query_var_defaults() {
        let source =
            std::fs::read_to_string("src/parser/tests/queries/query_var_defaults.graphql").unwrap();
        let minified_query = super::minify_query(&source).unwrap();
        insta::assert_snapshot!(minified_query, @"query Foo($site:Site=MOBILE){field}");
    }

    #[test]
    fn test_minify_query_vars() {
        let source =
            std::fs::read_to_string("src/parser/tests/queries/query_vars.graphql").unwrap();
        let minified_query = super::minify_query(&source).unwrap();
        insta::assert_snapshot!(minified_query, @"query Foo($arg:SomeType){field}");
    }

    #[test]
    fn test_minify_string_literal() {
        let source =
            std::fs::read_to_string("src/parser/tests/queries/string_literal.graphql").unwrap();
        let minified_query = super::minify_query(&source).unwrap();
        insta::assert_snapshot!(minified_query, @r#"query{node(id:"hello")}"#);
    }

    #[test]
    fn test_minify_subscription_directive() {
        let source =
            std::fs::read_to_string("src/parser/tests/queries/subscription_directive.graphql")
                .unwrap();
        let minified_query = super::minify_query(&source).unwrap();
        insta::assert_snapshot!(minified_query, @"subscription@directive{node}");
    }

    #[test]
    fn test_minify_triple_quoted_literal() {
        let source =
            std::fs::read_to_string("src/parser/tests/queries/triple_quoted_literal.graphql")
                .unwrap();
        let minified_query = super::minify_query(&source).unwrap();
        insta::assert_snapshot!(minified_query, @r#"
        query{node(id:"""
            Hello,
              world!
          """)}
        "#);
    }
}
