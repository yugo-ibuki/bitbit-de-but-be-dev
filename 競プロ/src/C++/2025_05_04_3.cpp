#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <map>
#include <algorithm>
using namespace std;
#define rep(i, a, b) for (int i = a; i < b; i++)

int main() {
  string W;
  cin >> W;

  char c;
  bool flag = true;
  for (int i = 0; i < W.size(); i++) {
    c = W[i];
    int count_l = count(W.begin(), W.end(), c);
    if (count_l % 2 != 0) {
      flag = false;
    }
  }

  if (flag) {
    cout << "Yes" << endl;
  } else {
    cout << "No" << endl;
  }
}


// 別解
// int main() {
//     string S;
//     cin >> S;

//     vector<int> con(26, 0);
//     for (int i = 0; i < S.size(); i++) {
//         con[S[i] - 'a']++;
//     }

//     bool res = true;
//     for (int i = 0; i < 26; ++i) if (con[i] % 2 == 1) res = false;

//     cout << (res ? "Yes" : "No") << endl;
// }
