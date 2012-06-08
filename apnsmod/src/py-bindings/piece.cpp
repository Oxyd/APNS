#include "piece.hpp"
#include "py-utils.hpp"

#include <boost/python.hpp>

static
boost::optional<apns::piece> py_piece_from_letter(std::string const& letter) {
  if (letter.length() == 1)
    return apns::piece_from_letter(letter[0]);
  else
    throw std::logic_error("Expected one-letter string");
}

static std::string py_letter_from_piece(apns::piece const& p) {
  return std::string(1, apns::letter_from_piece(p));
}

void export_piece() {
  using namespace boost::python;

    to_python_converter<boost::optional<apns::piece>, optional_to_T<apns::piece> >();

    {
      scope piece_scope =
          class_<apns::piece>("Piece",
            "Represents a single Arimaa game piece",
            init<apns::piece::color_t, apns::piece::type_t>())
            .add_property("color", &apns::piece::color)
            .add_property("type", &apns::piece::type)
            ;

      enum_<apns::piece::color_t>("Color",
        "Color of a piece. Either gold or silver.")
        .value("gold", apns::piece::gold)
        .value("silver", apns::piece::silver)
        ;

      enum_<apns::piece::type_t>("Type",
        "Type of the piece. In Arimaa, there are six types of pieces: elephant, camel, horse, dog, cat, and rabbit")
        .value("elephant", apns::piece::elephant)
        .value("camel", apns::piece::camel)
        .value("horse", apns::piece::horse)
        .value("dog", apns::piece::dog)
        .value("cat", apns::piece::cat)
        .value("rabbit", apns::piece::rabbit)
        ;
    }

    def("pieceFromLetter", &py_piece_from_letter,
        "pieceFromLetter(letter) -> Piece\n\nConstruct a piece from its Arimaa letter.");
    def("letterFromPiece", &py_letter_from_piece,
        "letterFromPiece(Piece) -> str\n\nGet the letter corresponding a to a piece.");

}
