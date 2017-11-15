#include <regex>
#include <string>

using std::string;

namespace ValidNumber {

class Solution {
  public:
    bool isNumber(string s) {
        return std::regex_match(s, r);
    }
    std::regex r{
        "^\\s*[+\\-]?(:?\\d+(:?\\.\\d*)?|\\d*\\.\\d+)(:?e[+\\-]?\\d+)?\\s*$",
        std::regex_constants::icase};
};

} // namespace ValidNumber