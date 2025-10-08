use rouille::{router, Request, Response};

use super::{parser_viz, synth_viz};

pub fn serve(bind_addr: &str) {
    println!("Starting visualization server on http://{}", bind_addr);
    println!("  - Parser visualizer: http://{}/", bind_addr);
    println!("  - Synthesis visualizer: http://{}/synth", bind_addr);
    
    rouille::start_server(bind_addr, move |request: &Request| {
        router!(request,
            (GET) (/) => {
                let html = include_str!("./static/index.html");
                Response::html(html)
            },
            (GET) (/synth) => {
                let html = include_str!("./static/synth.html");
                Response::html(html)
            },
            (POST) (/graph) => {
                parser_viz::handle_parser_viz_request(request)
            },
            (POST) (/synth) => {
                synth_viz::handle_synth_request(request)
            },
            (GET) (/static/{file: String}) => {
                serve_static_file(&file)
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
        "synth.js" => {
            let js = include_str!("./static/synth.js");
            Response::from_data("application/javascript", js.to_string())
        }
        "styles.css" => {
            let css = include_str!("./static/styles.css");
            Response::from_data("text/css", css.to_string())
        }
        _ => Response::empty_404(),
    }
}
