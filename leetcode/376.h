// A sequence of numbers is called a wiggle sequence if the differences between
// successive numbers strictly alternate between positive and negative. The
// first difference (if one exists) may be either positive or negative. A
// sequence with fewer than two elements is trivially a wiggle sequence.

// For example, [1,7,4,9,2,5] is a wiggle sequence because the differences
// (6,-3,5,-7,3) are alternately positive and negative. In contrast, [1,4,7,2,5]
// and [1,7,4,5,5] are not wiggle sequences, the first because its first two
// differences are positive and the second because its last difference is zero.

// Given a sequence of integers, return the length of the longest subsequence
// that is a wiggle sequence. A subsequence is obtained by deleting some number
// of elements (eventually, also zero) from the original sequence, leaving the
// remaining elements in their original order.

#include <algorithm>
#include <vector>

using namespace std;

namespace WiggleSubsequence {

// dummy O(n^2) solve, maintain 2 dp arrays
class Solution {
  public:
    int wiggleMaxLength(vector<int> &nums) {
        if (nums.empty()) {
            return 0;
        }
        vector<int> bigger(nums.size(), 1), smaller(nums.size(), 1);
        bigger.front() = smaller.front() = 1;
        int m = 1;
        for (int i = 1; i < nums.size(); ++i) {
            for (int j = 0; j < i; ++j) {
                if (nums[i] > nums[j]) {
                    bigger[i] = max(bigger[i], smaller[j] + 1);
                    m = max(m, bigger[i]);
                } else if (nums[i] < nums[j]) {
                    smaller[i] = max(smaller[i], bigger[j] + 1);
                    m = max(m, smaller[i]);
                }
            }
        }
        return m;
    }
};

// improved O(n) solution with O(1) extra space
// aims to find the local peak of the array
class Solution2 {
public:
    int wiggleMaxLength(vector<int>& nums) {
        if (nums.size() <= 1) {
            return nums.size();
        }
        int flag = 0, result = 1;
        for (int i = 1; i < nums.size(); ++i) {
            int temp = nums[i] - nums[i - 1];
            if (temp == 0) {
                continue;
            }
            if (temp * flag <= 0) {
                ++result;
                flag = temp;
            }
        }
        return result;
    }
};

}