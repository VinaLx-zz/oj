#include <array>
#include <bitset>
#include <iostream>
#include <set>
#include <utility>
#include <vector>

using std::array;
using std::bitset;
using std::pair;
using std::set;
using std::vector;

namespace SudokuSolver {

class Solution {
  public:
    using XY = pair<int, int>;
    using Board = vector<vector<char>>;
    array<array<set<XY>, 9>, 9> related;
    array<array<bitset<9>, 9>, 9> disabled;
    void preprocess(vector<vector<char>> &board) {
        related = {};
        disabled = {};
        for (int i = 0; i < 9; ++i) {
            for (int j = 0; j < 9; ++j) {
                int x = i / 3 * 3, y = j / 3 * 3;
                for (int k = 0; k < 9; ++k) {
                    if (k != i) {
                        related[i][j].insert({k, j});
                    }
                    if (k != j) {
                        related[i][j].insert({i, k});
                    }
                    int dx = k / 3, dy = k % 3;
                    if (x + dx != i || y + dy != j) {
                        related[i][j].insert({x + dx, y + dy});
                    }
                }
            }
        }
        for (int i = 0; i < 9; ++i) {
            for (int j = 0; j < 9; ++j) {
                if (board[i][j] == '.')
                    continue;
                int n = board[i][j] - '1';
                disabled[i][j] |= bitset<9>(0x1FF);
                for (const auto &xy : related[i][j]) {
                    related[xy.first][xy.second].erase({i, j});
                    disabled[xy.first][xy.second].set(n);
                }
                related[i][j].clear();
            }
        }
    }
    bool search(Board &board, XY xy = {-1, -1}) {
        if (xy.first == -1) {
            int max_disabled = 0;
            for (int i = 0; i < 9; ++i) {
                for (int j = 0; j < 9; ++j) {
                    if (board[i][j] != '.')
                        continue;
                    auto &d = disabled[i][j];
                    auto c = d.count();
                    if (c > max_disabled) {
                        max_disabled = c;
                        xy = {i, j};
                    }
                }
            }
        }
        int x = xy.first, y = xy.second;

        if (x == -1) {
            return true;
        }
        if (disabled[x][y].all()) {
            return false;
        }
        for (int n = 0; n < 9; ++n) {
            if (not disabled[x][y].test(n) && search(board, x, y, n)) {
                return true;
            }
        }
        return false;
    }
    bool search(Board &board, int x, int y, int n) {
        vector<XY> disable_affected;
        int max_disabled = 0;
        XY next(-1, -1);
        for (const auto &xy : related[x][y]) {
            related[xy.first][xy.second].erase({x, y});
            auto &d = disabled[xy.first][xy.second];
            if (!d.test(n)) {
                d.set(n);
                disable_affected.push_back(xy);
                if (d.count() > max_disabled) {
                    max_disabled = d.count();
                    next = xy;
                }
            }
        }
        board[x][y] = '1' + n;

        if (search(board, next)) {
            return true;
        }

        board[x][y] = '.';
        for (const auto &xy : related[x][y]) {
            related[xy.first][xy.second].insert({x, y});
        }
        for (const auto &xy : disable_affected) {
            disabled[xy.first][xy.second].reset(n);
        }
        return false;
    }
    void solveSudoku(Board &board) {
        preprocess(board);
        search(board);
    }
};
} // namespace SudokuSolver