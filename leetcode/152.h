// Find the contiguous subarray within an array (containing at least one number)
// which has the largest product.
//
// For example, given the array [2,3,-2,4],
// the contiguous subarray [2,3] has the largest product = 6.
//
// both method use dp, O(n), second code looks simpler

#include <algorithm>
#include <vector>

using std::vector;
using std::max;
using std::min;

namespace maximumProductSubarray {

class Solution {
  public:
    int maxProduct(vector<int>& nums) {
        vector<int> products(nums.size());  // maximum product till index
        products.front() = nums.front();
        // record the total product from last 0
        int product = nums.front(),  
            // the product that count from last 0 to the first minus number
            // after 0
            minus_product = nums.front() < 0 ? nums.front() : 0;
        for (int i = 1; i < nums.size(); ++i) {
            product *= nums[i];  // update product
            if (product == 0) {  // if last product is 0 or this number is 0
                product = nums[i];
                products[i] = max(products[i - 1], nums[i]);
                // update minus_product if neccessary
                minus_product = nums[i] < 0 ? nums[i] : 0;
            } else if (product < 0) {
                if (not minus_product) { // first minus after 0
                    products[i] = products[i - 1]; // inherit the max product
                    minus_product = product;
                } else {
                    // fall back, divide the minus_product to see whether it's bigger
                    products[i] = max(products[i - 1], product / minus_product);
                }
            } else {
                products[i] = max(products[i - 1], product); // product > 0, then update
            }
        }
        return products.back(); // last number is the answer
    }
};

// learn to use simpler dp, but doesn't optimize the time somehow...
class SolutionOptimized {
  public:
    int maxProduct(vector<int>& nums) {
        if (nums.size() == 1) {
            return nums.front();
        }
        int n = nums.size();

        int prev_max, prev_min, ans;
        prev_max = prev_min = ans = nums[0];

        for (int i = 1; i < n; ++i) {
            int prev_max_copy = prev_max, prev_min_copy = prev_min;

            if (nums[i] > 0) {
                prev_max = max(nums[i] * prev_max_copy, nums[i]);
                prev_min = min(nums[i] * prev_min_copy, nums[i]);
            } else {
                prev_max = max(nums[i] * prev_min_copy, nums[i]);
                prev_min = max(nums[i] * prev_max_copy, nums[i]);
            }
            ans = max(ans, prev_max);
        }

        return ans;
    }
};
};
