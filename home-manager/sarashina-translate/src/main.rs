use std::env;
use std::io::{self, IsTerminal, Read};
use std::time::Duration;

use clap::{Parser, ValueEnum};
use reqwest::blocking::Client;
use serde::{Deserialize, Serialize};

const DEFAULT_ENDPOINT: &str = "https://api.sarashina.ai/v1/translate";
const MAX_CODEPOINTS: usize = 5_000;

#[derive(Clone, Copy, Debug, Eq, PartialEq, ValueEnum)]
enum Language {
    En,
    Ja,
}

impl Language {
    fn as_str(self) -> &'static str {
        match self {
            Self::En => "en",
            Self::Ja => "ja",
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, ValueEnum)]
#[serde(rename_all = "lowercase")]
enum Style {
    Auto,
    Plain,
    Polite,
}

#[derive(Debug, Parser)]
#[command(
    version,
    about = "Translate Japanese and English with the Sarashina Translate API",
    after_help = "TEXT may be omitted or set to '-' to read from stdin.\n\
                  SARASHINA_API_KEY must contain the API key."
)]
struct Args {
    /// Source language.
    #[arg(long, value_enum, default_value = "en")]
    source: Language,

    /// Target language.
    #[arg(long, value_enum, default_value = "ja")]
    target: Language,

    /// Japanese output style. Valid only when --target ja.
    #[arg(long, value_enum)]
    style: Option<Style>,

    /// Text to translate. Multiple words are joined with spaces.
    #[arg(value_name = "TEXT", num_args = 0.., trailing_var_arg = true)]
    text: Vec<String>,
}

#[derive(Debug, Serialize)]
struct TranslateRequest<'a> {
    text: &'a str,
    source_lang: &'static str,
    target_lang: &'static str,
    #[serde(skip_serializing_if = "Option::is_none")]
    style: Option<Style>,
}

#[derive(Debug, Deserialize)]
struct TranslateResponse {
    translated_text: String,
}

#[derive(Debug, Deserialize)]
struct ErrorResponse {
    message: Option<String>,
}

fn input_text(args: &[String]) -> Result<String, String> {
    if args.is_empty() || args == ["-"] {
        if io::stdin().is_terminal() {
            return Err("pass TEXT as arguments or pipe it on stdin".into());
        }
        let mut text = String::new();
        io::stdin()
            .read_to_string(&mut text)
            .map_err(|error| format!("failed to read stdin: {error}"))?;
        Ok(text)
    } else {
        Ok(args.join(" "))
    }
}

fn validate(
    text: &str,
    source: Language,
    target: Language,
    style: Option<Style>,
) -> Result<(), String> {
    if text.is_empty() {
        return Err("translation text must not be empty".into());
    }
    let codepoints = text.chars().count();
    if codepoints > MAX_CODEPOINTS {
        return Err(format!(
            "translation text is {codepoints} code points; the API limit is {MAX_CODEPOINTS}"
        ));
    }
    if source == target {
        return Err("--source and --target must be different".into());
    }
    if style.is_some() && target != Language::Ja {
        return Err("--style is valid only when --target ja".into());
    }
    Ok(())
}

fn translate(
    endpoint: &str,
    api_key: &str,
    text: &str,
    source: Language,
    target: Language,
    style: Option<Style>,
) -> Result<String, String> {
    let request = TranslateRequest {
        text,
        source_lang: source.as_str(),
        target_lang: target.as_str(),
        style,
    };
    let client = Client::builder()
        .timeout(Duration::from_secs(60))
        .build()
        .map_err(|error| format!("failed to create HTTP client: {error}"))?;
    let response = client
        .post(endpoint)
        .bearer_auth(api_key)
        .json(&request)
        .send()
        .map_err(|error| format!("Sarashina API request failed: {error}"))?;
    let status = response.status();
    let body = response
        .text()
        .map_err(|error| format!("failed to read Sarashina API response: {error}"))?;

    if !status.is_success() {
        let message = serde_json::from_str::<ErrorResponse>(&body)
            .ok()
            .and_then(|error| error.message)
            .unwrap_or(body);
        return Err(format!("Sarashina API returned {status}: {message}"));
    }

    serde_json::from_str::<TranslateResponse>(&body)
        .map(|response| response.translated_text)
        .map_err(|error| format!("invalid Sarashina API response: {error}"))
}

fn run() -> Result<(), String> {
    let args = Args::parse();
    let text = input_text(&args.text)?;
    validate(&text, args.source, args.target, args.style)?;
    let api_key =
        env::var("SARASHINA_API_KEY").map_err(|_| "SARASHINA_API_KEY is not set".to_string())?;
    if api_key.is_empty() {
        return Err("SARASHINA_API_KEY is empty".into());
    }
    let endpoint =
        env::var("SARASHINA_TRANSLATE_ENDPOINT").unwrap_or_else(|_| DEFAULT_ENDPOINT.to_string());
    let translated = translate(
        &endpoint,
        &api_key,
        &text,
        args.source,
        args.target,
        args.style,
    )?;
    println!("{translated}");
    Ok(())
}

fn main() {
    if let Err(error) = run() {
        eprintln!("sarashina-translate: {error}");
        std::process::exit(1);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_request_is_english_to_japanese() {
        let request = TranslateRequest {
            text: "Hello",
            source_lang: Language::En.as_str(),
            target_lang: Language::Ja.as_str(),
            style: None,
        };
        assert_eq!(
            serde_json::to_value(request).unwrap(),
            serde_json::json!({
                "text": "Hello",
                "source_lang": "en",
                "target_lang": "ja"
            })
        );
    }

    #[test]
    fn validates_language_style_and_codepoint_limit() {
        assert!(validate("text", Language::En, Language::Ja, Some(Style::Plain)).is_ok());
        assert!(validate("text", Language::En, Language::En, None).is_err());
        assert!(validate("text", Language::Ja, Language::En, Some(Style::Auto)).is_err());
        assert!(validate(
            &"あ".repeat(MAX_CODEPOINTS),
            Language::Ja,
            Language::En,
            None
        )
        .is_ok());
        assert!(validate(
            &"あ".repeat(MAX_CODEPOINTS + 1),
            Language::Ja,
            Language::En,
            None
        )
        .is_err());
    }

    #[test]
    fn parses_success_and_error_responses() {
        let success: TranslateResponse =
            serde_json::from_str(r#"{"translated_text":"こんにちは","future":true}"#).unwrap();
        assert_eq!(success.translated_text, "こんにちは");

        let error: ErrorResponse = serde_json::from_str(r#"{"message":"Bad Request"}"#).unwrap();
        assert_eq!(error.message.as_deref(), Some("Bad Request"));
    }
}
