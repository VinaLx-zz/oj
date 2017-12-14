#include <vector>

using std::vector;

namespace Candy {

class Solution {
  public:
    int candy(vector<int> &ratings) {
        if (ratings.size() <= 1) {
            return ratings.size();
        }
        int result = 1;
        int down = 1, up = 1;
        bool prevDown = true;
        for (int i = 1; i < ratings.size(); ++i) {
            if (ratings[i] <= ratings[i - 1]) {
                if (prevDown == false) {
                    prevDown = true;
                    down = 1;
                } else if (++down == up) {
                    ++down;
                }
                if (ratings[i] == ratings[i - 1]) {
                    up = 1;
                    down = 1;
                }
                result += down;
            } else {
                if (prevDown) {
                    prevDown = false;
                    up = 1;
                }
                result += ++up;
            }
        }
        return result;
    }
};

} // namespace Candy