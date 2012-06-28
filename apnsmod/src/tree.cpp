#include "tree.hpp"

#include <boost/lexical_cast.hpp>
#include <boost/algorithm/string/trim.hpp>
#include <boost/bind.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/lambda/bind.hpp>
#include <boost/filesystem.hpp>
#include <boost/utility.hpp>

#include <algorithm>
#include <limits>
#include <memory>
#include <string>
#include <fstream>
#include <iostream>
#include <stack>
#include <utility>
#include <numeric>
#include <iostream>

namespace apns {

#ifndef _MSC_VER
vertex::number_t const vertex::max_num;
vertex::number_t const vertex::infty;
#endif

void vertex::pack() {
  children_container new_children;
  new_children.transfer(new_children.begin(), children_);
  children_.swap(new_children);
}

namespace {

/// Set the subtree size of a vertex, assuming the subtree sizes of its children are set correctly.
void compute_size(vertex& v) {
  namespace bl = boost::lambda;
  v.subtree_size = std::accumulate(v.begin(), v.end(), 1, bl::_1 + bl::bind(&vertex::subtree_size, bl::_2));
}

}

void calculate_sizes(vertex& root) {
  traverse_postorder(root, postorder(), &compute_size);
  compute_size(root);

#ifndef NDEBUG
  vertex_counter count;
  traverse(root, backtrack(), boost::ref(count));
  assert(root.subtree_size == count.count);
#endif
}

namespace {

//! A stop condition that just keeps calling op_ctrl.update() to see whether 
//! the algorithm should stop.
struct op_ctrl_stop_cond {
  explicit op_ctrl_stop_cond(operation_controller& op_ctrl) :
    op_ctrl(op_ctrl)
  { }

  bool operator () (vertex&) {
    op_ctrl.update();
    return op_ctrl.stop();
  }

private:
  operation_controller& op_ctrl;
};

void bad_format() {
  throw std::runtime_error("Bad file format");
}

//! Dump-a-vertex visitor.
struct printer {
  explicit printer(std::ostream& out) :
    out(out)
  { }

  void operator () (vertex& v) {
    out << (v.step ? v.step->to_string() : "lambda")
        << " : " << (v.type == vertex::type_or ? "or" : "and")
        << ' '   << v.steps_remaining
        << ' '   << (v.proof_number < vertex::infty ?
                     boost::lexical_cast<std::string>(v.proof_number) : 
                     "infty")
        << ' '   << (v.disproof_number < vertex::infty ?
                     boost::lexical_cast<std::string>(v.disproof_number) :
                     "infty")
        << ' '   << v.size()
        << '\n'
        ;
  }

private:
  std::ostream& out;
};

//! Load-a-vertex visitor.
struct reader {
  explicit reader(std::istream& in) :
    in(in)
  { }

  void operator () (vertex& v) {
    try {
      std::string token;

      if (!std::getline(in, token, ':')) bad_format();
      boost::algorithm::trim(token);
      boost::optional<step> maybe_step = step::from_string(token);
      if (maybe_step)
        v.step = *maybe_step;
      else if (token != "lambda" && token != "root") bad_format();

      if (!(in >> token)) bad_format();
      if (token == "or") v.type = vertex::type_or;
      else if (token == "and") v.type = vertex::type_and;
      else bad_format();

      vertex::number_t num_token;
      if (!(in >> num_token)) bad_format();
      if (num_token && num_token <= 4) v.steps_remaining = num_token;
      else bad_format();

      if (!(in >> token)) bad_format();
      if (token != "infty") 
        v.proof_number = boost::lexical_cast<vertex::number_t>(token);
      else v.proof_number = vertex::infty;

      if (!(in >> token)) bad_format();
      if (token != "infty")
        v.disproof_number = boost::lexical_cast<vertex::number_t>(token);
      else v.disproof_number = vertex::infty;

      if (!(in >> num_token)) bad_format();
      v.resize(num_token);
    } catch (boost::bad_lexical_cast&) {
      bad_format();
    }
  }

private:
  std::istream& in;
};

//! Delete a file given by its filename. This is inherently platform-specific.
void delete_file(std::string const& filename) {
  remove(boost::filesystem::path(filename));
}

} // anonymous namespace

void save_game(boost::shared_ptr<game> const& game, 
               std::string const& filename, operation_controller& op_ctrl) {
  std::ios_base::sync_with_stdio(false);

  std::ofstream out(filename.c_str());
  try {
    out.exceptions(std::ios_base::badbit | std::ios_base::failbit);
    
    out << string_from_board(game->initial_state);
    out << '\n';
    if (game->attacker == piece::gold)
      out << "gold";
    else
      out << "silver";
    out << '\n';

    traverse(game->root, backtrack(), printer(out), op_ctrl_stop_cond(op_ctrl));

    if (op_ctrl.stop()) {
      // Cancelled -- the file has been only partially written -- so delete
      // it.
      out.close();
      delete_file(filename);
    }

  } catch (std::ios_base::failure& f) {
    delete_file(filename);  // The file has only been partially written.
    throw std::runtime_error(f.what());
  }
}

std::pair<boost::shared_ptr<game>, std::size_t>
load_game(std::string const& filename, operation_controller& op_ctrl) {
  std::ios_base::sync_with_stdio(false);

  std::ifstream in(filename.c_str());
  in.exceptions(std::ios_base::badbit);

  std::string line;
  if (!std::getline(in, line)) bad_format();
  board initial_state;
  board_from_string(line, initial_state);

  if (!std::getline(in, line)) bad_format();
  boost::algorithm::trim(line);

  piece::color_t attacker;
  if (line == "gold")         attacker = piece::gold;
  else if (line == "silver")  attacker = piece::silver;
  else bad_format();

  boost::shared_ptr<apns::game> game(new apns::game(initial_state, attacker));
  traverse(game->root, backtrack(), reader(in), op_ctrl_stop_cond(op_ctrl));
  compute_size(game->root);

  if (!op_ctrl.stop())
    return std::make_pair(game, game->root.subtree_size);
  else
    return std::make_pair(boost::shared_ptr<apns::game>(), 0);
}

} // namespace apns

