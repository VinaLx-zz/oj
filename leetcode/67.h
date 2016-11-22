// binary high precision

#include <string>

using std::string;

namespace addBinary {

class Solution {
public:
    string addBinary(string a, string b) {
        string result(a.size() > b.size() ? a.size() + 1 : b.size() + 1, '0');
        int64_t i = a.size() - 1, j = b.size() - 1, k = result.size() - 1;
        for (; i >= 0 or j >= 0; --i, --j, --k) {
            if (i >= 0) {
                result[k] += a[i] - '0';
            }
            if (j >= 0) {
                result[k] += b[j] - '0';
            }
            if (result[k] >= '2') {
                result[k] -= 2;
                ++result[k - 1];
            }
        }
        size_t start = 0;
        for (; start < result.size() and result[start] == '0'; ++start) continue;
        if (start == result.size()) {
            return "0";
        }
        return result.substr(start);
    }
};

} // namespace addBinary
