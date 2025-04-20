// abc042 A - Iroha and Haiku (ABC Edition)
#include <bits/stdc++.h>
#include <atcoder/all>
using namespace std;
using namespace atcoder;

/**
日本の誇る美しいリズムとして、五七五というものがあります。いろはちゃんは五七五が大好きです。

3 つの文節の並びの長さがそれぞれ 
5,7,5 となるようにこの順番で並んでいるとき、その 
3 つの文節の並びは五七五であると言います。

並び替えたい 
3 つの文節の長さを表す整数 
A,B,C が与えられるので、それらの文節を並び替えて五七五にできるか判定してください。
 */
int main() {
  int a, b, c;
  cin >> a >> b >> c;
  bool is_haiku = false;
  if (a == 5 && b == 7 && c == 5) {
    is_haiku = true;
  }
  if (a == 5 && b == 5 && c == 7) {
    is_haiku = true;
  }
  if (a == 7 && b == 5 && c == 5) {
    is_haiku = true;
  }
  cout << (is_haiku ? "YES" : "NO") << endl;
}
