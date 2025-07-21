# Intersection Observer API 完全ガイド

## Intersection Observer とは

Intersection Observer API は、対象要素とその親要素またはビューポートとの交差状態を非同期で監視するWebAPIです。従来のスクロールイベントによる監視と比べて、パフォーマンスが大幅に向上し、より効率的な要素の可視性判定が可能になります。

## 主な使用場面

### 1. 遅延読み込み（Lazy Loading）

**画像の遅延読み込み**
```typescript
const observer = new IntersectionObserver((entries) => {
  entries.forEach(entry => {
    if (entry.isIntersecting) {
      const img = entry.target as HTMLImageElement;
      img.src = img.dataset.src!;
      observer.unobserve(img);
    }
  });
});

document.querySelectorAll('img[data-src]').forEach(img => {
  observer.observe(img);
});
```

**動的コンテンツの読み込み**
- API からのデータ取得
- 大きなリストの分割読み込み
- 動画や音声ファイルの遅延読み込み

### 2. 無限スクロール

```typescript
const observer = new IntersectionObserver((entries) => {
  entries.forEach(entry => {
    if (entry.isIntersecting) {
      loadMoreContent();
    }
  });
}, { threshold: 1.0 });

// ページ最下部の要素を監視
observer.observe(document.querySelector('.load-more-trigger'));
```

### 3. アニメーション・エフェクト

**スクロールに応じたアニメーション**
```typescript
const observer = new IntersectionObserver((entries) => {
  entries.forEach(entry => {
    if (entry.isIntersecting) {
      entry.target.classList.add('animate');
    }
  });
}, { threshold: 0.5 });
```

**パララックス効果**
```typescript
const observer = new IntersectionObserver((entries) => {
  entries.forEach(entry => {
    const ratio = entry.intersectionRatio;
    const element = entry.target as HTMLElement;
    element.style.transform = `translateY(${(1 - ratio) * 100}px)`;
  });
}, { threshold: Array.from({length: 101}, (_, i) => i / 100) });
```

### 4. アナリティクス・トラッキング

**要素の表示時間測定**
```typescript
const viewTimeTracker = new Map();

const observer = new IntersectionObserver((entries) => {
  entries.forEach(entry => {
    const elementId = entry.target.id;
    
    if (entry.isIntersecting) {
      viewTimeTracker.set(elementId, Date.now());
    } else {
      const startTime = viewTimeTracker.get(elementId);
      if (startTime) {
        const viewTime = Date.now() - startTime;
        analytics.track('element_viewed', {
          elementId,
          viewTime,
          intersectionRatio: entry.intersectionRatio
        });
      }
    }
  });
});
```

### 5. ナビゲーション・目次

**アクティブセクションのハイライト**
```typescript
const sections = document.querySelectorAll('section[id]');
const navLinks = document.querySelectorAll('nav a[href^="#"]');

const observer = new IntersectionObserver((entries) => {
  entries.forEach(entry => {
    const id = entry.target.id;
    const navLink = document.querySelector(`nav a[href="#${id}"]`);
    
    if (entry.isIntersecting) {
      navLink?.classList.add('active');
    } else {
      navLink?.classList.remove('active');
    }
  });
}, { threshold: 0.7 });

sections.forEach(section => observer.observe(section));
```

### 6. パフォーマンス最適化

**不要な処理の停止**
```typescript
const observer = new IntersectionObserver((entries) => {
  entries.forEach(entry => {
    const video = entry.target as HTMLVideoElement;
    
    if (entry.isIntersecting) {
      video.play();
      startExpensiveAnimation(video);
    } else {
      video.pause();
      stopExpensiveAnimation(video);
    }
  });
});
```

## 設定オプション詳細

### root
監視の基準となる要素。`null` の場合はビューポートが使用されます。

```typescript
// 特定の要素を基準にする
const container = document.getElementById('scroll-container');
const observer = new IntersectionObserver(callback, {
  root: container
});
```

### rootMargin
root の周囲のマージン。CSS の margin 記法で指定。

```typescript
const observer = new IntersectionObserver(callback, {
  rootMargin: '100px 0px -50px 0px' // 上100px、右0px、下-50px、左0px
});
```

### threshold
コールバックが実行される交差比率。

```typescript
const observer = new IntersectionObserver(callback, {
  threshold: [0, 0.25, 0.5, 0.75, 1.0] // 複数の閾値
});
```

## 実践的なユースケース

### 1. 高性能な画像ギャラリー

```typescript
class LazyImageGallery {
  private observer: IntersectionObserver;
  
  constructor() {
    this.observer = new IntersectionObserver(
      this.handleIntersection.bind(this),
      {
        rootMargin: '50px',
        threshold: 0.1
      }
    );
    
    this.init();
  }
  
  private handleIntersection(entries: IntersectionObserverEntry[]) {
    entries.forEach(entry => {
      if (entry.isIntersecting) {
        this.loadImage(entry.target as HTMLImageElement);
        this.observer.unobserve(entry.target);
      }
    });
  }
  
  private loadImage(img: HTMLImageElement) {
    const src = img.dataset.src;
    if (src) {
      img.src = src;
      img.onload = () => img.classList.add('loaded');
    }
  }
  
  private init() {
    document.querySelectorAll('img[data-src]')
      .forEach(img => this.observer.observe(img));
  }
}
```

### 2. スクロール進捗インジケーター

```typescript
class ScrollProgressIndicator {
  private observer: IntersectionObserver;
  private progressBar: HTMLElement;
  
  constructor(progressBarSelector: string) {
    this.progressBar = document.querySelector(progressBarSelector)!;
    
    this.observer = new IntersectionObserver(
      this.updateProgress.bind(this),
      {
        threshold: Array.from({length: 101}, (_, i) => i / 100)
      }
    );
    
    this.init();
  }
  
  private updateProgress(entries: IntersectionObserverEntry[]) {
    entries.forEach(entry => {
      const progress = entry.intersectionRatio * 100;
      this.progressBar.style.width = `${progress}%`;
    });
  }
  
  private init() {
    const mainContent = document.querySelector('main');
    if (mainContent) {
      this.observer.observe(mainContent);
    }
  }
}
```

### 3. 仮想スクロール

```typescript
class VirtualScroll {
  private observer: IntersectionObserver;
  private itemHeight: number;
  private visibleCount: number;
  
  constructor(container: HTMLElement, itemHeight: number) {
    this.itemHeight = itemHeight;
    this.visibleCount = Math.ceil(container.clientHeight / itemHeight) + 2;
    
    this.observer = new IntersectionObserver(
      this.handleIntersection.bind(this),
      {
        root: container,
        rootMargin: `${itemHeight}px`
      }
    );
  }
  
  private handleIntersection(entries: IntersectionObserverEntry[]) {
    entries.forEach(entry => {
      if (entry.isIntersecting) {
        this.renderVisibleItems(entry.target);
      } else {
        this.unrenderItem(entry.target);
      }
    });
  }
  
  private renderVisibleItems(sentinel: Element) {
    // 可視領域のアイテムのみをレンダリング
  }
  
  private unrenderItem(item: Element) {
    // 非可視アイテムをDOMから削除してメモリを節約
  }
}
```

## パフォーマンスのベストプラクティス

### 1. 適切な閾値の設定
```typescript
// ❌ 不要に細かい閾値
threshold: Array.from({length: 1001}, (_, i) => i / 1000)

// ✅ 必要最小限の閾値
threshold: [0, 0.5, 1.0]
```

### 2. observer の再利用
```typescript
// ❌ 要素ごとに新しい observer を作成
elements.forEach(el => {
  const observer = new IntersectionObserver(callback);
  observer.observe(el);
});

// ✅ 一つの observer で複数要素を監視
const observer = new IntersectionObserver(callback);
elements.forEach(el => observer.observe(el));
```

### 3. 不要な監視の停止
```typescript
// 一度だけ実行したい処理の場合
const observer = new IntersectionObserver((entries) => {
  entries.forEach(entry => {
    if (entry.isIntersecting) {
      doSomething();
      observer.unobserve(entry.target); // 監視を停止
    }
  });
});
```

## ブラウザサポートと代替案

### サポート状況
- Chrome 51+
- Firefox 55+
- Safari 12.1+
- Edge 15+

### ポリフィル
```typescript
// ポリフィルの読み込み
if (!('IntersectionObserver' in window)) {
  import('intersection-observer').then(() => {
    // IntersectionObserver を使用
  });
}
```

### 代替実装（古いブラウザ用）
```typescript
function fallbackVisibilityCheck(element: Element) {
  const rect = element.getBoundingClientRect();
  return (
    rect.top >= 0 &&
    rect.left >= 0 &&
    rect.bottom <= window.innerHeight &&
    rect.right <= window.innerWidth
  );
}
```

## まとめ

Intersection Observer API は現代のウェブ開発において非常に重要なツールです。適切に使用することで：

- **パフォーマンスの向上**: スクロールイベントより効率的
- **ユーザー体験の改善**: スムーズなアニメーションと遅延読み込み
- **メモリ使用量の削減**: 不要な処理の停止
- **アクセシビリティの向上**: 適切なタイミングでの要素読み込み

これらの利点を活かして、より良いウェブアプリケーションを構築しましょう。