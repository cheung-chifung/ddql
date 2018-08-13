use std::sync::Arc;
use rusoto_core::Region;
use rusoto_dynamodb::{DynamoDb, DynamoDbClient, ListTablesInput};
use parser::Query;

pub struct Executor {
    pub client: Arc<DynamoDbClient>,
}

impl Executor {
    pub fn new(c: DynamoDbClient) -> Self {
        Executor {
            client: Arc::new(c),
        }
    }

    pub fn execute_query(&self, q: Query) {
        println!("{}", q);
    }
}
