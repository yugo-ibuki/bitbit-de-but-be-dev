// https://atcoder.jp/contests/abc042/tasks
// B
#include <bits/stdc++.h>
#include <atcoder/all>
using namespace std;
using namespace atcoder;

int main() {
  int n, l;
  cin >> n >> l;
  vector<string> s(n);
  for (int i = 0; i < n; i++) {
    cin >> s[i];
  }

  sort(s.begin(), s.end());
  string ans = "";
  for (int i = 0; i < n; i++) {
    ans += s[i];
  }
  cout << ans << endl;
}
