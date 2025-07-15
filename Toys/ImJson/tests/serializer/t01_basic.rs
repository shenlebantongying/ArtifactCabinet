use im_json::serializer::*;

#[test]
fn create_list() {
    let mut ser = Serializer::new();

    ser.begin_list();
    {
        ser.num(1);
        ser.num(2);
        ser.num(3);
    }
    ser.end_list();

    assert_eq!(ser.to_string_compact(), "[1,2,3]");
}

#[test]
fn key_value() {
    let mut ser = Serializer::new();

    ser.begin_object();
    {
        ser.begin_member_key("hello");
        ser.begin_list();
        {
            ser.num(1);
        }
        ser.end_list();
        ser.begin_member_key("world");
        {
            ser.begin_list();
            {
                ser.num(2);
                ser.num(3);
                ser.num(4);
            }
            ser.end_list();
        }
    }
    ser.end_object();

    assert_eq!(ser.to_string_compact(), "{hello:[1],world:[2,3,4]}");
}
