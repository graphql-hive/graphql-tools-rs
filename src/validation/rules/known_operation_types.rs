use super::ValidationRule;
use crate::ast::{    visit_document, OperationVisitor, OperationVisitorContext, SchemaDocumentExtension,
    };
use crate::static_graphql::query::*;
use crate::validation::utils::{ValidationError, ValidationErrorContext};

/// Known operation types
///
/// A GraphQL operation is only valid if the operation type is within the schema.
///
/// See https://github.com/graphql/graphql-spec/pull/947
pub struct KnownOperationTypes;

impl KnownOperationTypes {
    pub fn new() -> Self {
        KnownOperationTypes
    }
}

fn build_error_message(operation_type: &str) -> String {
  format!("The {} operation is not supported by the schema.", operation_type)
}

impl<'a> OperationVisitor<'a, ValidationErrorContext> for KnownOperationTypes {
    fn enter_operation_definition(
        &mut self,
        visitor_context: &mut OperationVisitorContext,
        user_context: &mut ValidationErrorContext,
        operation_definition: &OperationDefinition,
    ) {
        match operation_definition {
            OperationDefinition::Mutation(mutation) => {
                if let None = visitor_context.schema.mutation_type() {
                    user_context.report_error(ValidationError {
                        locations: vec![mutation.position],
                        message: build_error_message("mutation"),
                    });
                }
            },
            OperationDefinition::Subscription(subscription) => {
                if let None = visitor_context.schema.subscription_type() {
                    user_context.report_error(ValidationError {
                        locations: vec![subscription.position],
                        message: build_error_message("subscription"),
                    });
                }
            },
            _ => {}
        }
    }
}

impl ValidationRule for KnownOperationTypes {
    fn validate<'a>(
        &self,
        ctx: &'a mut OperationVisitorContext,
        error_collector: &mut ValidationErrorContext,
    ) {
        visit_document(
            &mut KnownOperationTypes::new(),
            &ctx.operation,
            ctx,
            error_collector,
        );
    }
}

#[test]
fn one_known_operation() {
    use crate::validation::test_utils::*;

    let mut plan = create_plan_from_rule(Box::new(KnownOperationTypes {}));
    let errors = test_operation_with_schema(
        "{ field }",
        TEST_SCHEMA,
        &mut plan,
    );

    assert_eq!(get_messages(&errors).len(), 0);
}

#[test]
fn unknown_mutation_operation() {
    use crate::validation::test_utils::*;

    let mut plan = create_plan_from_rule(Box::new(KnownOperationTypes {}));
    let errors = test_operation_with_schema(
        "mutation { field }",
        "type Query { _: String }",
        &mut plan,
    );

    let messages = get_messages(&errors);
    assert_eq!(messages.len(), 1);
    assert_eq!(
        messages,
        vec!["The mutation operation is not supported by the schema."]
    );
}

#[test]
fn unknown_subscription_operation() {
    use crate::validation::test_utils::*;

    let mut plan = create_plan_from_rule(Box::new(KnownOperationTypes {}));
    let errors = test_operation_with_schema(
        "subscription { field }",
        "type Query { _: String }",
        &mut plan,
    );

    let messages = get_messages(&errors);
    assert_eq!(messages.len(), 1);
    assert_eq!(
        messages,
        vec!["The subscription operation is not supported by the schema."]
    );
}

#[test]
fn mixture_of_known_and_unknown_operations() {
    use crate::validation::test_utils::*;

    let mut plan = create_plan_from_rule(Box::new(KnownOperationTypes {}));
    let errors = test_operation_with_schema(
        "query { field }
        mutation { field }
        subscription { field }",
        "type Query { field: String }",
        &mut plan,
    );

    let messages = get_messages(&errors);
    assert_eq!(messages.len(), 2);
    assert_eq!(
        messages,
        vec!["The mutation operation is not supported by the schema.", "The subscription operation is not supported by the schema."]
    );
}
