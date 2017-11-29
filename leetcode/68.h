#include <string>
#include <vector>

using std::string;
using std::vector;

namespace TextJustification {

class Solution {
  public:
    vector<string> fullJustify(vector<string> &words, int maxWidth) {
        int start = 0, end = 0;
        vector<string> result;
        do {
            ++end;
            int l = words[start].size();
            int c = 0;
            for (; end < words.size() and
                   l + words[end].size() + c + 1 <= maxWidth;) {
                l += words[end].size();
                ++end;
                ++c;
            }
            result.push_back("");
            result.back().reserve(maxWidth);
            result.back() += words[start++];

            if (c == 0) {
                result.back() += string(maxWidth - l, ' ');
            } else {
                int sep = (maxWidth - l) / c;
                int rem = (maxWidth - l) % c;
                for (; start < end; ++start) {
                    int t = end == words.size() ? 1 : sep + (rem-- > 0 ? 1 : 0);
                    result.back() += string(t, ' ');
                    result.back() += words[start];
                }
                if (end == words.size()) {
                    result.back() += string(maxWidth - result.back().size(), ' ');
                }
            }
        } while (end < words.size());
        return result;
    }
};

} // namespace TextJustification