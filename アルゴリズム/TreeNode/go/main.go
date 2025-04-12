// Goでの木構造の実装
package main

import (
	"fmt"
)

// TreeNode は木構造のノードを表す
type TreeNode struct {
	Value    string
	Children []*TreeNode
}

// NewTreeNode は新しいTreeNodeを作成する
func NewTreeNode(value string) *TreeNode {
	return &TreeNode{
		Value:    value,
		Children: []*TreeNode{},
	}
}

// AddChild は子ノードを追加する
func (n *TreeNode) AddChild(value string) *TreeNode {
	child := NewTreeNode(value)
	n.Children = append(n.Children, child)
	return child
}

// DepthFirstTraversal は深さ優先探索を行う
func (n *TreeNode) DepthFirstTraversal(callback func(string)) {
	// 現在のノードを処理
	callback(n.Value)
	
	// 子ノードを再帰的に処理
	for _, child := range n.Children {
		child.DepthFirstTraversal(callback)
	}
}

// BreadthFirstTraversal は幅優先探索を行う
func (n *TreeNode) BreadthFirstTraversal(callback func(string)) {
	// キューを使用
	queue := []*TreeNode{n}
	
	for len(queue) > 0 {
		// キューから先頭のノードを取り出す
		currentNode := queue[0]
		queue = queue[1:]
		
		// 現在のノードを処理
		callback(currentNode.Value)
		
		// 子ノードをキューに追加
		queue = append(queue, currentNode.Children...)
	}
}

// FindNode は特定の値を持つノードを探す（深さ優先探索）
func (n *TreeNode) FindNode(value string) *TreeNode {
	// 現在のノードが探している値を持っている場合
	if n.Value == value {
		return n
	}

	// 子ノードを探索
	for _, child := range n.Children {
		found := child.FindNode(value)
		if found != nil {
			return found
		}
	}

	// 見つからなかった場合
	return nil
}

// 使用例
func main() {
	// ルートノードを作成
	root := NewTreeNode("Root")

	// 子ノードを追加
	node1 := root.AddChild("Node 1")
	node2 := root.AddChild("Node 2")
	// node3 := root.AddChild("Node 3")

	// さらに深い階層を追加
	node1.AddChild("Node 1.1")
	node1.AddChild("Node 1.2")
	node2.AddChild("Node 2.1")
	node22 := node2.AddChild("Node 2.2")
	node22.AddChild("Node 2.2.1")

	// 深さ優先探索の例
	fmt.Println("深さ優先探索:")
	root.DepthFirstTraversal(func(value string) {
		fmt.Println(value)
	})

	// 幅優先探索の例
	fmt.Println("\n幅優先探索:")
	root.BreadthFirstTraversal(func(value string) {
		fmt.Println(value)
	})

	// ノード検索の例
	fmt.Println("\nノード検索:")
	foundNode := root.FindNode("Node 2.2")
	if foundNode != nil {
		fmt.Printf("ノード \"%s\" が見つかりました！\n", foundNode.Value)
	} else {
		fmt.Println("ノードが見つかりませんでした")
	}
}