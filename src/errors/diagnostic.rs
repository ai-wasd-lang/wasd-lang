//! Diagnostic reporting for WASD compiler errors.

#![allow(dead_code)]

use ariadne::{Color, Label, Report, ReportKind, Source};

/// The kind of diagnostic.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticKind {
    Error,
    Warning,
    Info,
}

/// A compiler diagnostic.
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub kind: DiagnosticKind,
    pub message: String,
    pub span: (usize, usize),
    pub labels: Vec<(String, (usize, usize))>,
    pub notes: Vec<String>,
}

impl Diagnostic {
    /// Create a new error diagnostic.
    pub fn error(message: impl Into<String>, span: (usize, usize)) -> Self {
        Self {
            kind: DiagnosticKind::Error,
            message: message.into(),
            span,
            labels: Vec::new(),
            notes: Vec::new(),
        }
    }

    /// Create a new warning diagnostic.
    pub fn warning(message: impl Into<String>, span: (usize, usize)) -> Self {
        Self {
            kind: DiagnosticKind::Warning,
            message: message.into(),
            span,
            labels: Vec::new(),
            notes: Vec::new(),
        }
    }

    /// Add a label to this diagnostic.
    pub fn with_label(mut self, message: impl Into<String>, span: (usize, usize)) -> Self {
        self.labels.push((message.into(), span));
        self
    }

    /// Add a note to this diagnostic.
    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }
}

/// Report an error to stderr using ariadne.
pub fn report_error(filename: &str, source: &str, diagnostic: &Diagnostic) {
    let kind = match diagnostic.kind {
        DiagnosticKind::Error => ReportKind::Error,
        DiagnosticKind::Warning => ReportKind::Warning,
        DiagnosticKind::Info => ReportKind::Advice,
    };

    let color = match diagnostic.kind {
        DiagnosticKind::Error => Color::Red,
        DiagnosticKind::Warning => Color::Yellow,
        DiagnosticKind::Info => Color::Blue,
    };

    let span = (filename, diagnostic.span.0..diagnostic.span.1);

    let mut report = Report::build(kind, span.clone())
        .with_message(&diagnostic.message)
        .with_label(
            Label::new(span)
                .with_message(&diagnostic.message)
                .with_color(color),
        );

    for (msg, (start, end)) in &diagnostic.labels {
        report = report.with_label(
            Label::new((filename, *start..*end))
                .with_message(msg)
                .with_color(Color::Cyan),
        );
    }

    for note in &diagnostic.notes {
        report = report.with_note(note);
    }

    report
        .finish()
        .eprint((filename, Source::from(source)))
        .unwrap();
}

/// Report multiple diagnostics.
#[allow(dead_code)]
pub fn report_diagnostics(filename: &str, source: &str, diagnostics: &[Diagnostic]) {
    for diagnostic in diagnostics {
        report_error(filename, source, diagnostic);
    }
}
