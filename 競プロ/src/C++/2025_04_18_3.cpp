#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
using namespace std;

// C++/2025_04_18_2.cpp のコードを参考にしています

/**
 * 問題文：
 * 縦 H 行、横 W 列のマス目があります。上から i 行目、左から j 列目のマスを (i,j) と表します。
 * 各マスは白または黒です。マス目の配色は、H 行 W 列の行列 (a_i,j) によって与えられます。
 * a_i,j が '.' ならばマス (i,j) は白であり、a_i,j が '#' ならばマス (i,j) は黒です。
 * 
 * すぬけ君はこのマス目を圧縮しようとしています。そのために、白いマスのみからなる行または列が
 * 存在する間、次の操作を繰り返し行います。
 * 
 * 操作: 白いマスのみからなる行または列をひとつ任意に選び、その行または列を取り除いて空白を詰める。
 * 
 * 各操作でどの行または列を選ぶかによらず、最終的なマス目は一意に定まることが示せます。
 * 最終的なマス目を求めてください。
 */

// グリッドの状態
int H, W;
string grid[101];  // 最大サイズは101

/**
 * 圧縮処理を1回行う関数
 * 白マスのみの行または列を1つ見つけて削除する
 * 
 * 戻り値:
 *   true: 圧縮を行った（白マスのみの行または列を見つけて削除した）
 *   false: 圧縮を行わなかった（白マスのみの行または列がなかった）
 */
bool compress_once() {
    // 1. まず行をチェック - 白マスのみの行を探す
    for (int y = 0; y < H; y++) {
        bool is_all_white = true;  // この行が全て白マスかどうか
        
        // 行の各マスをチェック
        for (int x = 0; x < W; x++) {
            if (grid[y][x] == '#') {  // 黒マスがあれば
                is_all_white = false;  // 全て白マスではない
                break;
            }
        }
        
        // 白マスのみの行が見つかった場合
        if (is_all_white) {
            // この行を削除（下の行を上に詰める）
            for (int yy = y + 1; yy < H; yy++) {
                swap(grid[yy - 1], grid[yy]);
            }
            H--;  // 行数を1減らす
            return true;  // 圧縮を行った
        }
    }
    
    // 2. 次に列をチェック - 白マスのみの列を探す
    for (int x = 0; x < W; x++) {
        bool is_all_white = true;  // この列が全て白マスかどうか
        
        // 列の各マスをチェック
        for (int y = 0; y < H; y++) {
            if (grid[y][x] == '#') {  // 黒マスがあれば
                is_all_white = false;  // 全て白マスではない
                break;
            }
        }
        
        // 白マスのみの列が見つかった場合
        if (is_all_white) {
            // この列を削除（各行から該当する文字を削除）
            for (int y = 0; y < H; y++) {
                // x列目を除いた新しい文字列を作成
                // 前半（0からx-1まで）+ 後半（x+1から最後まで）
                grid[y] = grid[y].substr(0, x) + grid[y].substr(x + 1);
            }
            W--;  // 列数を1減らす
            return true;  // 圧縮を行った
        }
    }
    
    // 白マスのみの行も列も見つからなかった
    return false;
}

int main() {
    // 入力を読み込む
    cin >> H >> W;
    for (int i = 0; i < H; i++) {
        cin >> grid[i];
    }
    
    // 圧縮処理を繰り返す
    while (compress_once()) {
        // 白マスのみの行または列がなくなるまで繰り返す
    }
    
    // 結果を出力
    for (int y = 0; y < H; y++) {
        cout << grid[y] << endl;
    }
    
    return 0;
}
