#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <map>
using namespace std;
#define rep(i, a, b) for (int i = a; i < b; i++)

int main() {
  string N;
  cin >> N;

  vector<string> S(N.size());
  rep(i, 0, N.size()) S[i] = N[i];

  map<string, int> mp;
  rep(i, 0, N.size()) {
    mp[S[i]]++;
  };

  int max_value = 0;
  string max_key = "";
  for (auto [key, value] : mp) {
    if (max_value < value) {
      max_key = key;
    }
    max_value = max(max_value, value);
  }

  int start = 0;
  int end = 0;
  for (int i = 0; i < N.size(); i++) {
    
  }

  cout << max_key << endl;
  cout << max_value << endl;
}
