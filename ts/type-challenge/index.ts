type Length<T extends string[] | number[]> = T['length'];

// 使用例:
type tesla = ['tesla1', 'tesla2', 'tesla3', 'tesla4']
type spaceX = ['falcon', 'falcon heavy', 'dragon', 'starhip', 'human']

type teslaLength = Length<tesla> // 4
type spaceXLength = Length<spaceX> // 5