#include <iostream>  // 入出力のためのヘッダーファイル
#include <string>    // 文字列操作のためのヘッダーファイル
#include <vector>    // ベクターのためのヘッダーファイル
using namespace std;

/**
問題文
N 個の都市があり、
M 本の道路があります。
i(1≦i≦M) 番目の道路は、都市 a_i と 都市 b_i 
(1≦a_i, b_i≦N) を双方向に結んでいます。
同じ 2 つの都市を結ぶ道路は、
1 本とは限りません。
各都市から他の都市に向けて、何本の道路が伸びているか求めてください。

https://atcoder.jp/contests/abc061/editorial

https://drken1215.hatenablog.com/entry/2025/01/01/070329
 */
int main() {
  int N, M;
  cin >> N >> M;

  vector<vector<int>> graph(N);
  for (int i = 0; i < M; i++) {
    int a, b;
    cin >> a >> b;
    a--, b--;

    graph[a].push_back(b);
    graph[b].push_back(a);
  }

  for (int i = 0; i < N; i++) {
    cout << graph[i].size() << endl;
  }
}
