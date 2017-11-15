#include <algorithm>
#include <cstddef>
#include <limits>

namespace BinaryTreeMaximumPathSum {

struct TreeNode {
    int val;
    TreeNode *left;
    TreeNode *right;
    TreeNode(int x) : val(x), left(NULL), right(NULL) {}
};

class Solution {
  public:
    int maxPathSum(TreeNode *root) {
        max = std::numeric_limits<int>().min();
        search(root);
        return max;
    }
    int search(TreeNode *node) {
        if (node == nullptr) {
            return std::numeric_limits<int>().min();
        }
        int lm = node->left == nullptr ? 0 : search(node->left),
            rm = node->right == nullptr ? 0 : search(node->right),
            n = node->val;
        max = std::max(
            max, std::max(n, std::max(n + lm + rm, std::max(n + lm, n + rm))));
        return std::max(n, std::max(n + lm, n + rm));
    }
    int max;
};

} // namespace BinaryTreeMaximumPathSum