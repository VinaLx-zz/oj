#include <string>
#include <utility>

using std::move;
using std::string;

class Solution {
  public:
    void skipSpaces() {
        for (; n < s.size() && s[n] == ' '; ++n)
            continue;
    }
    int number() {
        int acc = 0;
        for (; n < s.size() && isdigit(s[n]); ++n) {
            acc = acc * 10 + s[n] - '0';
        }
        return acc;
    }
    bool op() {
        return s[n++] == '+';
    }
    int primary() {
        skipSpaces();
        int acc;
        if (s[n] == '(') {
            ++n;
            acc = primary();
        } else {
            acc = number();
        }
        for (;;) {
            skipSpaces();
            if (n == s.size()) {
                break;
            }
            if (s[n] == ')') {
                ++n;
                break;
            }
            auto o = op();
            skipSpaces();
            int next;
            if (s[n] == '(') {
                ++n;
                next = primary();
            } else {
                next = number();
            }
            acc = o ? acc + next : acc - next;
        }
        return acc;
    }

    int calculate(string ss) {
        n = 0;
        s = move(ss);
        return primary();
    }
    int n;
    string s;
};
