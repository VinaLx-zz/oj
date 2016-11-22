// Two Sum
//
// Given an array of integers that is already sorted in ascending order, find
// two numbers such that they add up to a specific target number.
//
// The function twoSum should return indices of the two numbers such that they
// add up to the target, where index1 must be less than index2. Please note that
// your returned answers (both index1 and index2) are not zero-based.
//
// You may assume that each input would have exactly one solution.
//
// Input: numbers={2, 7, 11, 15}, target=9
// Output: {1, 2}

#include <utility>
#include <vector>

using std::vector;

// 6ms solution
// binary search for both i and j
class Solution1 {
  public:
    vector<int> twoSum(vector<int> &numbers, int target) {
        return solve(numbers, 0, numbers.size(), target);
    }

    vector<int>
    solve(const vector<int> &nums, int ilow, int ihigh, int target) {
        if (ilow == ihigh)
            return {};
        int imid = (ihigh + ilow) / 2;
        if (nums[imid] > target)
            return solve(nums, ilow, imid, target);

        int left = target - nums[imid];
        int jhigh, jlow;
        if (left < nums[imid]) {
            jhigh = imid;
            jlow = 0;
        } else {
            jhigh = nums.size();
            jlow = imid + 1;
        }
        for (;;) {
            if (jhigh == jlow)
                break;
            int jmid = (jhigh + jlow) / 2;
            if (nums[jmid] == left) {
                vector<int> result{imid + 1, jmid + 1};
                if (imid > jmid)
                    std::swap(result.front(), result.back());
                return result;
            }
            if (nums[jmid] < left)
                jlow = jmid + 1;
            else
                jhigh = jmid;
        }

        auto ans = solve(nums, ilow, imid, target);
        if (ans.size())
            return ans;
        return solve(nums, imid + 1, ihigh, target);
    }
};

// 9ms solution
// linear search for i, binary search for j
// (don't really know why this is slower)
class Solution2 {
  public:
    vector<int> twoSum(vector<int> &numbers, int target) {
        for (int i = 0; i < numbers.size(); ++i) {
            int left = target - numbers[i];
            int jlow(i + 1), jhigh(numbers.size());
            for (;;) {
                if (jlow == jhigh)
                    break;
                int jmid = (jhigh + jlow) / 2;
                if (numbers[jmid] == left) {
                    return {i + 1, jmid + 1};
                }
                if (numbers[jmid] < left)
                    jlow = jmid + 1;
                else
                    jhigh = jmid;
            }
        }
        return {};
    }
};
