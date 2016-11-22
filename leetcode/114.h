#include <cstddef>

namespace FlattenBinaryTreeToLinkedList {

// Definition for a binary tree node.
struct TreeNode {
    int val;
    TreeNode* left;
    TreeNode* right;
    TreeNode(int x) : val(x), left(NULL), right(NULL) {}
};

class Solution {
  public:
    void flatten(TreeNode* root) {
        flatten_helper(root);
    }

  private:
    TreeNode* flatten_helper(TreeNode* node) {
        if (not node) {
            return nullptr;
        }
        TreeNode *left_end = nullptr, *right_end = nullptr;
        left_end = flatten_helper(node->left);
        right_end = flatten_helper(node->right);
        if (not left_end and not right_end) {
            return node;
        }
        if (not left_end) {
            return right_end;
        }
        left_end->right = node->right;
        node->right = node->left;
        node->left = nullptr;
        if (not right_end) {
            return left_end;
        }
        return right_end;
    }
};

}  // namespace FlattenBinaryTreeToLinkedList
