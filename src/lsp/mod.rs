//! WASD Language Server Protocol implementation.
//!
//! Provides IDE support through LSP including:
//! - Diagnostics (type errors, parse errors)
//! - Hover information (future)
//! - Go to definition (future)

use crate::parser::Parser;
use crate::types::TypeChecker;
use std::collections::HashMap;
use std::sync::Mutex;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

/// The WASD language server.
pub struct WasdLanguageServer {
    client: Client,
    documents: Mutex<HashMap<Url, String>>,
}

impl WasdLanguageServer {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: Mutex::new(HashMap::new()),
        }
    }

    /// Analyze a document and publish diagnostics.
    async fn analyze_document(&self, uri: &Url, text: &str) {
        let mut diagnostics = Vec::new();

        // Parse
        let mut parser = Parser::new(text);
        match parser.parse() {
            Ok(program) => {
                // Type check
                let mut type_checker = TypeChecker::new();
                if let Err(errors) = type_checker.check_program(&program) {
                    for error in errors {
                        diagnostics.push(Diagnostic {
                            range: Range {
                                start: Position {
                                    line: 0,
                                    character: 0,
                                },
                                end: Position {
                                    line: 0,
                                    character: 1,
                                },
                            },
                            severity: Some(DiagnosticSeverity::ERROR),
                            source: Some("wasd".to_string()),
                            message: error,
                            ..Default::default()
                        });
                    }
                }
            }
            Err(error) => {
                diagnostics.push(Diagnostic {
                    range: Range {
                        start: Position {
                            line: 0,
                            character: 0,
                        },
                        end: Position {
                            line: 0,
                            character: 1,
                        },
                    },
                    severity: Some(DiagnosticSeverity::ERROR),
                    source: Some("wasd".to_string()),
                    message: format!("Parse error: {}", error),
                    ..Default::default()
                });
            }
        }

        self.client
            .publish_diagnostics(uri.clone(), diagnostics, None)
            .await;
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for WasdLanguageServer {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "wasd-lsp".to_string(),
                version: Some("0.1.0".to_string()),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "WASD language server initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let text = params.text_document.text;

        {
            let mut docs = self.documents.lock().unwrap();
            docs.insert(uri.clone(), text.clone());
        }

        self.analyze_document(&uri, &text).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;

        if let Some(change) = params.content_changes.into_iter().next() {
            {
                let mut docs = self.documents.lock().unwrap();
                docs.insert(uri.clone(), change.text.clone());
            }

            self.analyze_document(&uri, &change.text).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let mut docs = self.documents.lock().unwrap();
        docs.remove(&params.text_document.uri);
    }
}

/// Run the language server.
pub async fn run_server() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(WasdLanguageServer::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}
