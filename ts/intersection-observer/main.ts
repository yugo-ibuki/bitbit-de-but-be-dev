interface ObservedElement {
  element: Element;
  id: string;
  isVisible: boolean;
}

class IntersectionObserverPlayground {
  private observer!: IntersectionObserver;
  private observedElements: Map<string, ObservedElement> = new Map();
  private statusList: HTMLElement | null = null;

  constructor() {
    this.initializeObserver();
    this.setupStatusPanel();
    this.observeElements();
  }

  private initializeObserver(): void {
    const options: IntersectionObserverInit = {
      root: null, // ビューポートをルートとして使用
      rootMargin: '0px',
      threshold: [0, 0.25, 0.5, 0.75, 1.0] // 複数の閾値を設定
    };

    this.observer = new IntersectionObserver(
      this.handleIntersection.bind(this),
      options
    );
  }

  private handleIntersection(entries: IntersectionObserverEntry[]): void {
    entries.forEach((entry: IntersectionObserverEntry) => {
      const target = entry.target as HTMLElement;
      const elementId = target.dataset.id || target.id;
      
      console.log(`Element ${elementId}:`, {
        isIntersecting: entry.isIntersecting,
        intersectionRatio: entry.intersectionRatio,
        boundingClientRect: entry.boundingClientRect,
        intersectionRect: entry.intersectionRect
      });

      this.updateElementVisibility(target, entry.isIntersecting, entry.intersectionRatio);
      this.updateStatusPanel(elementId, entry.isIntersecting);
      
      // 特別な処理
      this.handleSpecialEffects(target, entry);
    });
  }

  private updateElementVisibility(
    element: HTMLElement, 
    isVisible: boolean, 
    ratio: number
  ): void {
    if (isVisible) {
      element.classList.add('visible');
    } else {
      element.classList.remove('visible');
    }

    // 交差比率に基づいた動的スタイリング
    if (ratio > 0.5) {
      element.style.opacity = '1';
    } else if (ratio > 0) {
      element.style.opacity = (0.3 + ratio * 0.7).toString();
    }
  }

  private handleSpecialEffects(element: HTMLElement, entry: IntersectionObserverEntry): void {
    // フェードインアニメーション
    if (element.classList.contains('fade-in') && entry.isIntersecting) {
      element.classList.add('visible');
    }

    // 遅延画像読み込み
    if (element.classList.contains('lazy-load') && entry.isIntersecting) {
      this.loadLazyImage(element);
    }

    // カスタムイベントの発火
    if (entry.isIntersecting) {
      const customEvent = new CustomEvent('elementVisible', {
        detail: {
          element: element,
          intersectionRatio: entry.intersectionRatio,
          elementId: element.dataset.id
        }
      });
      document.dispatchEvent(customEvent);
    }
  }

  private loadLazyImage(container: HTMLElement): void {
    const placeholder = container.querySelector('.image-placeholder') as HTMLElement;
    if (!placeholder) return;

    const imageSrc = placeholder.dataset.src;
    if (!imageSrc) return;

    const img = document.createElement('img');
    img.src = imageSrc;
    img.alt = 'Lazy loaded image';
    
    img.onload = () => {
      img.classList.add('loaded');
      placeholder.innerHTML = '';
      placeholder.appendChild(img);
    };

    img.onerror = () => {
      placeholder.innerHTML = '<span>画像の読み込みに失敗しました</span>';
    };
  }

  private setupStatusPanel(): void {
    this.statusList = document.getElementById('status-list');
    
    // カスタムイベントリスナーの設定
    document.addEventListener('elementVisible', (event: Event) => {
      const customEvent = event as CustomEvent;
      console.log('Custom event fired:', customEvent.detail);
    });
  }

  private updateStatusPanel(elementId: string, isVisible: boolean): void {
    if (!this.statusList) return;

    let statusItem = document.getElementById(`status-${elementId}`);
    
    if (!statusItem) {
      statusItem = document.createElement('li');
      statusItem.id = `status-${elementId}`;
      statusItem.innerHTML = `
        <span>${elementId}</span>
        <div class="status-indicator"></div>
      `;
      this.statusList.appendChild(statusItem);
    }

    const indicator = statusItem.querySelector('.status-indicator') as HTMLElement;
    if (indicator) {
      if (isVisible) {
        indicator.classList.add('visible');
      } else {
        indicator.classList.remove('visible');
      }
    }

    // observedElementsマップの更新
    const observedElement = this.observedElements.get(elementId);
    if (observedElement) {
      observedElement.isVisible = isVisible;
    }
  }

  private observeElements(): void {
    const elements = document.querySelectorAll('.observed-section');
    
    elements.forEach((element: Element) => {
      const elementId = (element as HTMLElement).dataset.id || element.id || `element-${Date.now()}`;
      
      this.observedElements.set(elementId, {
        element: element,
        id: elementId,
        isVisible: false
      });

      this.observer.observe(element);
    });

    console.log(`Started observing ${elements.length} elements`);
  }

  // パブリックメソッド
  public addElement(element: Element, id?: string): void {
    const elementId = id || (element as HTMLElement).dataset.id || `element-${Date.now()}`;
    
    this.observedElements.set(elementId, {
      element: element,
      id: elementId,
      isVisible: false
    });

    this.observer.observe(element);
  }

  public removeElement(elementId: string): void {
    const observedElement = this.observedElements.get(elementId);
    if (observedElement) {
      this.observer.unobserve(observedElement.element);
      this.observedElements.delete(elementId);
      
      // ステータスパネルからも削除
      const statusItem = document.getElementById(`status-${elementId}`);
      if (statusItem) {
        statusItem.remove();
      }
    }
  }

  public getVisibleElements(): ObservedElement[] {
    return Array.from(this.observedElements.values()).filter(el => el.isVisible);
  }

  public disconnect(): void {
    this.observer.disconnect();
    this.observedElements.clear();
  }

  // デバッグ用メソッド
  public logStatus(): void {
    console.log('Current observation status:');
    this.observedElements.forEach((element, id) => {
      console.log(`${id}: ${element.isVisible ? 'visible' : 'hidden'}`);
    });
  }
}

// DOM読み込み完了後に初期化
document.addEventListener('DOMContentLoaded', () => {
  const playground = new IntersectionObserverPlayground();
  
  // グローバルスコープに公開（デバッグ用）
  (window as any).playground = playground;
  
  // キーボードショートカット
  document.addEventListener('keydown', (event: KeyboardEvent) => {
    if (event.ctrlKey && event.key === 'l') {
      event.preventDefault();
      playground.logStatus();
    }
  });

  console.log('Intersection Observer Playground initialized!');
  console.log('Press Ctrl+L to log current observation status');
});

// 型定義をエクスポート（コメントアウト）
// export type { ObservedElement };
// export { IntersectionObserverPlayground };