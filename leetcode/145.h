#include <stack>
#include <utility>
#include <vector>

using std::pair;
using std::stack;
using std::vector;

namespace BinaryTreePostorderTraversal {

struct TreeNode {
    int val;
    TreeNode *left;
    TreeNode *right;
};

class Solution {
  public:
    vector<int> postorderTraversal(TreeNode *root) {
        stack<pair<int, TreeNode *>> stk;
        vector<int> result;
        stk.push({0, root});
        for (; not stk.empty();) {
            if (stk.top().second == nullptr) {
                stk.pop();
                continue;
            }
            int count = stk.top().first;
            if (count == 2) {
                result.push_back(stk.top().second->val);
                stk.pop();
            } else {
                ++stk.top().first;
                if (count == 0) {
                    stk.push({
                        0,
                        stk.top().second->left,
                    });
                } else if (count == 1) {
                    stk.push({
                        0,
                        stk.top().second->right,
                    });
                } else {
                    throw "impossible";
                }
            }
        }
        return result;
    }
};

} // namespace BinaryTreePostorderTraversal