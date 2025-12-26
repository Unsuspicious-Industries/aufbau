use rouille::{Request, Response, router};

use super::viz;

pub fn serve(bind_addr: &str) {
    println!("Starting visualization server on http://{}", bind_addr);
    println!(" - Parser visualizer: http://{}/", bind_addr);

    rouille::start_server(bind_addr, move |request: &Request| {
        router!(request,
            (GET) (/) => {
                let html = include_str!("./static/index.html");
                Response::html(html)
            },
            (GET) (/specs) => {
                viz::handle_list_specs(request)
            },
            (POST) (/graph) => {
                viz::handle_parser_viz_request(request)
            },
            (POST) (/analyze) => {
                viz::handle_analyze_request(request)
            },
            (GET) (/static/{file: String}) => {
                serve_static_file(&file)
            },
            (GET) (/examples/{file: String}) => {
                serve_example_spec(&file)
            },
            _ => Response::empty_404()
        )
    });
}

fn serve_static_file(file: &str) -> Response {
    match file {
        "app.js" => {
            let js = include_str!("./static/app.js");
            Response::from_data("application/javascript", js.to_string())
        }
        "styles.css" => {
            let css = include_str!("./static/styles.css");
            Response::from_data("text/css", css.to_string())
        }
        _ => Response::empty_404(),
    }
}

fn serve_example_spec(file: &str) -> Response {
    // Simple allowlist: `examples/*.spec` only.
    if file.contains("/") || file.contains("..") {
        return Response::empty_404();
    }
    if !file.ends_with(".spec") {
        return Response::empty_404();
    }

    let path = format!("examples/{file}");
    match std::fs::read_to_string(&path) {
        Ok(s) => Response::from_data("text/plain", s),
        Err(_) => Response::empty_404(),
    }
}
