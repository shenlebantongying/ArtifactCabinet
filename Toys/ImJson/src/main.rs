use im_json::J;

fn main() {
    let tjson = include_str!("../t.json");

    let mut j = J::new(tjson);

    while let Some(s) = j.get_next_token() {
        println!("{s}");
    }
}
