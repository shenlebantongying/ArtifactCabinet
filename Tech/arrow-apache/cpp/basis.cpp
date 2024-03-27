#include <arrow/api.h>
#include <arrow/compute/api.h>

#include <iostream>
#include <print>

arrow::Status arrowBasis() {
  arrow::Int8Builder id_builder;
  arrow::StringBuilder name_builder;

  ARROW_RETURN_NOT_OK(id_builder.Append(1));
  ARROW_RETURN_NOT_OK(id_builder.Append(2));
  ARROW_RETURN_NOT_OK(name_builder.Append("tom"));
  ARROW_RETURN_NOT_OK(name_builder.Append("jery"));

  std::shared_ptr<arrow::Array> id;
  std::shared_ptr<arrow::Array> name;

  ARROW_ASSIGN_OR_RAISE(id, id_builder.Finish());
  ARROW_ASSIGN_OR_RAISE(name, name_builder.Finish());

  std::shared_ptr<arrow::Field> field_id = field("id", arrow::int8());
  std::shared_ptr<arrow::Field> field_name =
      arrow::field("name", arrow::utf8());

  std::shared_ptr<arrow::Schema> schema = arrow::schema({field_id, field_name});

  std::shared_ptr<arrow::RecordBatch> myrbatch =
      arrow::RecordBatch::Make(schema, id->length(), {id, name});

  arrow::Datum totalID;

  ARROW_ASSIGN_OR_RAISE(totalID,
                        arrow::compute::Sum({myrbatch->GetColumnByName("id")}));

  std::print("{}", myrbatch->ToString());

  std::print("totalID -> {}", totalID.ToString());

  return arrow::Status::OK();
}

int main() {
  auto ret = arrowBasis();

  std::cout << ret.message();

  return 0;
}
