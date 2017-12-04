#include <vector>

using std::vector;

namespace PatchingArray {

class Solution {
public:
    int minPatches(vector<int>& nums, int n) {
        int result = 0, reach = 0;
        for (int i = 0; reach < n;) {
            if (i < nums.size() and nums[i] <= reach + 1) {
                reach += nums[i++];
            } else {
                reach += reach + 1;
                ++result;
            }
        }
        return result;
    }
};

} // namespace PatchingArray