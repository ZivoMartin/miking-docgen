use axum::{
    Router,
    extract::Path,
    http::{StatusCode, header},
    response::{IntoResponse, Response},
    routing::get,
    serve,
};
use pulldown_cmark::{Parser, html};
use std::{fs, net::SocketAddr, path::PathBuf};
use tokio::net::TcpListener;

#[tokio::main]
async fn main() {
    let app = Router::new().route("/*path", get(serve_markdown));

    let addr = SocketAddr::from(([127, 0, 0, 1], 3000));
    let listener = TcpListener::bind(addr).await.unwrap();

    println!("Serveur démarré sur http://{}", addr);

    serve(listener, app).await.unwrap();
}

async fn serve_markdown(Path(path): Path<String>) -> Response {
    let mut file_path = PathBuf::from("static");
    file_path.push(&path);
    println!("{file_path:?}");
    match fs::read_to_string(&file_path) {
        Ok(markdown) => {
            let parser = Parser::new(&markdown);
            let mut html_output = String::new();
            html::push_html(&mut html_output, parser);

            let full_html = format!(
                "<!DOCTYPE html>
                <html>
                    <head>
                        <meta charset=\"utf-8\">
                        <title>{}</title>
                        <style>
                            body {{ font-family: sans-serif; max-width: 800px; margin: auto; padding: 2em; }}
                            h1, h2, h3 {{ color: #444; }}
                            pre, code {{ background: #f4f4f4; padding: 0.2em 0.4em; }}
                        </style>
                    </head>
                    <body>{}</body>
                </html>",
                file_path.file_name().unwrap().to_string_lossy(),
                html_output
            );

            (
                [(header::CONTENT_TYPE, "text/html; charset=utf-8")],
                full_html,
            )
                .into_response()
        }
        Err(_) => (
            StatusCode::NOT_FOUND,
            format!("Fichier non trouvé: {}", file_path.display()),
        )
            .into_response(),
    }
}
