#include <iostream>
#include <vector>
#include <string>
#include <algorithm>

using namespace std;

// C++/2025_04_18_1.cpp のコードを参考にしています

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

// グリッドの最大サイズ
const int MAX_SIZE = 101;

// グリッドの状態
int height, width;
string grid[MAX_SIZE];

// 文字が白マス('.')かどうかを判定
bool isWhiteCell(char cell) {
    return cell == '.';
}

// 文字が黒マス('#')かどうかを判定
bool isBlackCell(char cell) {
    return cell == '#';
}

// 指定された行が白マスのみで構成されているかチェック
bool isRowAllWhite(int rowIndex) {
    for (int x = 0; x < width; x++) {
        if (isBlackCell(grid[rowIndex][x])) {
            return false;
        }
    }
    return true;
}

// 指定された列が白マスのみで構成されているかチェック
bool isColumnAllWhite(int colIndex) {
    for (int y = 0; y < height; y++) {
        if (isBlackCell(grid[y][colIndex])) {
            return false;
        }
    }
    return true;
}

// 行を削除する処理
void removeRow(int rowIndex) {
    // 削除する行より下の行を1つずつ上に移動
    for (int y = rowIndex + 1; y < height; y++) {
        swap(grid[y - 1], grid[y]);
    }
    // グリッドの高さを1減らす
    height--;
}

// 列を削除する処理
void removeColumn(int colIndex) {
    for (int y = 0; y < height; y++) {
        // 列を削除するために、その列を除いた新しい文字列を作成
        string newRow;
        
        // 削除する列の前までの部分
        newRow += grid[y].substr(0, colIndex);
        
        // 削除する列の後ろの部分
        if (colIndex + 1 < grid[y].length()) {
            newRow += grid[y].substr(colIndex + 1);
        }
        
        // 新しい行で元の行を置き換え
        grid[y] = newRow;
    }
    // グリッドの幅を1減らす
    width--;
}

// 白マスのみの行または列を探して削除する
// 戻り値: 削除を行った場合はtrue、行わなかった場合はfalse
bool compressGrid() {
    // まず行をチェック
    for (int y = 0; y < height; y++) {
        if (isRowAllWhite(y)) {
            removeRow(y);
            return true;
        }
    }
    
    // 次に列をチェック
    for (int x = 0; x < width; x++) {
        if (isColumnAllWhite(x)) {
            removeColumn(x);
            return true;
        }
    }
    
    // 削除できる行も列もなかった
    return false;
}

// グリッドを表示
void printGrid() {
    for (int y = 0; y < height; y++) {
        cout << grid[y] << endl;
    }
}

int main() {
    // 入力を読み込む
    cin >> height >> width;
    for (int i = 0; i < height; i++) {
        cin >> grid[i];
    }
    
    // 圧縮処理を繰り返す
    while (compressGrid()) {
        // 白マスのみの行または列がなくなるまで繰り返す
    }
    
    // 結果を出力
    printGrid();
    
    return 0;
}
