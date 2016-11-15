#include <string>
#include <vector>

using namespace std;

// simple window solution
namespace LongestSubstringWithoutRepeatingCharacters {

class SolutionSimple {
  public:
    int lengthOfLongestSubstring(string s) {
        vector<bool> char_map(256, false);
        int result(0);
        for (int head(0), tail(0); tail < s.size(); ++head) {
            if (s.size() - head <= result)
                break;

            // head catch up with tail
            if (head == tail) {
                char_map[s[head]] = true;
                ++tail;
            }

            // expand the window
            for (; tail < s.size(); ++tail) {
                if (char_map[s[tail]])
                    break;
                char_map[s[tail]] = true;
            }

            // cout << head << ' ' << tail << '\n';
            result = max(result, tail - head);

            // move the head
            char_map[s[head]] = false;
        }
        return result;
    }
};

// optimized jump window solution
class SolutionOptimized {
  public:
    int lengthOfLongestSubstring(string s) {
        vector<int> char_map(256, -1);
        int result(0);
        for (int head(0), tail(0); tail < s.size();) {
            if (s.size() - head <= result)
                break;
            if (head == tail) {
                char_map[s[head]] = head;
                ++tail;
            }
            for (; tail < s.size(); ++tail) {
                if (char_map[s[tail]] >= head) {
                    result = max(result, tail - head);
                    head = char_map[s[tail]] + 1;
                    char_map[s[tail]] = tail;
                    ++tail;
                    break;
                }
                char_map[s[tail]] = tail;
            }
            result = max(result, tail - head);
        }
        return result;
    }
};

} // namespace LongestSubstringWithoutRepeatingCharacters