#include <limits>
#include <queue>
#include <unordered_set>
#include <utility>
#include <vector>
#include <iostream>

using std::max;
using std::numeric_limits;
using std::pair;
using std::queue;
using std::unordered_set;
using std::vector;
using std::cout;

namespace LongestIncreasingPath {

struct Hash {
    template <typename T, typename U>
    size_t operator()(const pair<T, U> &p) const {
        std::hash<T> ht;
        std::hash<U> hu;
        return ht(p.first) * 13 + hu(p.second) * 7;
    }
};

class Solution {
public:
  int longestIncreasingPath(vector<vector<int>> &matrix) {
      constexpr pair<int, int> D[] = {{0, 1}, {0, -1}, {1, 0}, {-1, 0}};
      if (matrix.empty() or matrix[0].empty()) {
          return 0;
      }
      unordered_set<pair<int, int>, Hash> s;
      queue<pair<int, int>> q;
      vector<vector<int>> result(
          matrix.size(), vector<int>(matrix[0].size(), 1));
      for (int i = 0; i < matrix.size(); ++i) {
          for (int j = 0; j < matrix[i].size(); ++j) {
              int n = matrix[i][j];
              bool b = true;
              for (const auto &d : D) {
                  if (Inside(matrix, i + d.first, j + d.second) and
                      matrix[i][j] < n) {
                      b = false;
                      break;
                  }
              }
              if (b) {
                  s.insert({i, j});
                  q.push({i, j});
              }
          }
      }
      for (; not q.empty();) {
          auto p = q.front();
          q.pop();
          s.erase(p);
          int n = matrix[p.first][p.second];
          for (const auto &d : D) {
              int x = p.first + d.first, y = p.second + d.second;
              if (not Inside(matrix, x, y) or matrix[x][y] <= n or
                  result[x][y] >= result[p.first][p.second] + 1) {
                  continue;
              }
              result[x][y] = result[p.first][p.second] + 1;
              if (not s.count({x, y})) {
                  q.push({x, y});
                  s.insert({x, y});
              }
          }
      }
      int m = 1;
      for (const auto &row : result) {
          for (auto l : row) {
              m = max(m, l);
          }
      }
      return m;
  }
  bool Inside(const vector<vector<int>> &matrix, int x, int y) {
      return x >= 0 and x < matrix.size() and y >= 0 and
             y < matrix[x].size();
  }
};
class Solution2 {
  public:
    int longestIncreasingPath(vector<vector<int>> &matrix) {
        constexpr pair<int, int> D[] = {{0, 1}, {0, -1}, {1, 0}, {-1, 0}};
        if (matrix.empty() or matrix[0].empty()) {
            return 0;
        }
        vector<pair<int, int>> start;
        vector<vector<int>> result(
            matrix.size(), vector<int>(matrix[0].size(), -1));
        for (int i = 0; i < matrix.size(); ++i) {
            for (int j = 0; j < matrix[i].size(); ++j) {
                int n = matrix[i][j];
                bool b = true;
                for (const auto &d : D) {
                    if (Inside(matrix, i + d.first, j + d.second) and
                        matrix[i][j] < n) {
                        b = false;
                        break;
                    }
                }
                if (b) {
                    start.push_back({i, j});
                }
            }
        }
        int m = 1;
        for (const auto p : start) {
            m = max(m, DFS(result, matrix, p.first, p.second));
        }
        for (const auto& row : result) {
            for (auto r : row) {
                cout << r << ' ';
            }
            cout << '\n';
        }
        return m;
    }
    int DFS(vector<vector<int>>& result, const vector<vector<int>>& matrix, int x, int y) {
        constexpr pair<int, int> D[] = {{0, 1}, {0, -1}, {1, 0}, {-1, 0}};
        if (result[x][y] > 0) {
            return result[x][y];
        }
        int m = 1;
        for (auto p : D) {
            int x2 = x + p.first, y2 = y + p.second;
            if (not Inside(matrix, x2, y2) or matrix[x2][y2] <= matrix[x][y]) {
                continue;
            }
            m = max(m, 1 + DFS(result, matrix, x2, y2));
        }
        return result[x][y] = m;
    }
    bool Inside(const vector<vector<int>> &matrix, int x, int y) {
        return x >= 0 and x < matrix.size() and y >= 0 and
               y < matrix[x].size();
    }
};

} // namespace LongestIncreasingPath