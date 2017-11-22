#include <algorithm>
#include <cstddef>

namespace RecoverBinaryTree {

struct TreeNode {
    int val;
    TreeNode *left;
    TreeNode *right;
    TreeNode(int x) : val(x), left(NULL), right(NULL) {}
};

class Solution {
  public:
    void recoverTree(TreeNode *root) {
        TreeNode *a = nullptr, *b = nullptr, *prev = nullptr;
        search(root, prev, a, b);
        std::swap(a->val, b->val);
    }
    void search(TreeNode *now, TreeNode *&prev, TreeNode *&a, TreeNode *&b) {
        if (now == nullptr) {
            return;
        }
        search(now->left, prev, a, b);
        if (prev != nullptr and prev->val > now->val) {
            if (a == nullptr) {
                a = prev;
                b = now;
            } else {
                b = now;
            }
        }
        prev = now;
        search(now->right, prev, a, b);
    }
};

} // namespace RecoverBinaryTree