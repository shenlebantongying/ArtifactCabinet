use im_json::deserializer::Deserializer;

#[test]
fn ok() {
    let mut j = Deserializer::new(include_str!("t01_list_basic.json"));

    j.expect_begin_array();
    let Some(v1) = j.expect_i64() else { panic!() };
    let Some(v2) = j.expect_i64() else { panic!() };
    j.expect_end_array();

    assert_eq!(v1, 123);
    assert_eq!(v2, 456);
}
