// Given a set of distinct positive integers, find the largest subset such that
// every pair (Si, Sj) of elements in this subset satisfies: Si % Sj = 0 or Sj %
// Si = 0.

// If there are multiple solutions, return any subset is fine.

// Example 1:

// nums: [1,2,3]

// Result: [1,2] (of course, [1,3] will also be ok)

// Example 2:

// nums: [1,2,4,8]

// Result: [1,2,4,8]

#include <algorithm>
#include <vector>

using std::vector;

namespace LargestDivisibleSubset {

// dp with O(n^2) time
class Solution {
  public:
    vector<int> largestDivisibleSubset(vector<int> &nums) {
        if (nums.empty()) {
            return {};
        }
        sort(nums.begin(), nums.end());
        vector<int> prev(nums.size(), 0), largest(nums.size(), 0);
        for (int i = 0; i < nums.size(); ++i) {
            for (int j = 0; j < i; ++j) {
                if (nums[i] % nums[j] == 0 and largest[i] < largest[j] + 1) {
                    largest[i] = largest[j] + 1;
                    prev[i] = j;
                }
            }
            if (largest[i] == 0) {
                largest[i] = 1;
                prev[i] = i;
            }
        }
        int max_size = largest.front(), max_index = 0;
        for (int i = 1; i < nums.size(); ++i) {
            if (largest[i] > max_size) {
                max_size = largest[i];
                max_index = i;
            }
        }
        vector<int> ret;
        for (; max_index != prev[max_index]; max_index = prev[max_index]) {
            ret.push_back(nums[max_index]);
        }
        ret.push_back(nums[max_index]);
        return ret;
    }
};
}