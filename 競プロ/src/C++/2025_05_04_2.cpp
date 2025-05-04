#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <map>
using namespace std;
#define rep(i, a, b) for (int i = a; i < b; i++)

int main() {
  int N, K, X, Y;
  cin >> N >> K >> X >> Y;

  int ans = 0;
  rep(i, 0, N) {
    if (i < K) {
      ans += X;
    } else {
      ans += Y;
    }
  }

  cout << ans << endl;
}

// 別解 (forではなく、ifで解く)
// int main() {
//   int N, K, X, Y;
//   cin >> N >> K >> X >> Y;
  
//   int res;
//   if (N <= K) {
//       // N 泊が K 泊以内である場合は、X 円 × N 日
//       res = X * N;
//   } else {
//       // N 泊が超える場合は、最初の K 泊は X 円 x K 日。残りは Y 円 × (N - K) 日
//       res = X * K + Y * (N - K);
//   }
//   cout << res << endl;
// }