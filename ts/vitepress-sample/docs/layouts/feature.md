---
layout: home
---

# Feature Page Layout

<div class="feature-section">
  <div class="feature-content">
    <h2>Powerful Documentation Framework</h2>
    <p>
      Build beautiful, fast, and SEO-friendly documentation sites with minimal configuration.
      VitePress combines the power of Vue.js and Vite to deliver an exceptional developer experience.
    </p>
    <div class="feature-buttons">
      <a href="/guide/" class="feature-button primary">Get Started</a>
      <a href="https://github.com/vuejs/vitepress" class="feature-button secondary">GitHub</a>
    </div>
  </div>
  <div class="feature-image">
    <img src="https://vitepress.dev/vitepress-logo-large.webp" alt="VitePress">
  </div>
</div>

## Key Features

<div class="features-grid">
  <div class="feature-card">
    <div class="feature-icon">⚡️</div>
    <h3>Lightning Fast</h3>
    <p>Built on top of Vite, enjoy instant server start and lightning-fast HMR.</p>
  </div>
  
  <div class="feature-card">
    <div class="feature-icon">🛠️</div>
    <h3>Customizable</h3>
    <p>Extend the default theme or create your own with Vue components.</p>
  </div>
  
  <div class="feature-card">
    <div class="feature-icon">📝</div>
    <h3>Markdown-Centered</h3>
    <p>Focus on writing content with an enhanced Markdown experience.</p>
  </div>
  
  <div class="feature-card">
    <div class="feature-icon">🔍</div>
    <h3>SEO Friendly</h3>
    <p>Optimized for search engines out of the box.</p>
  </div>
</div>

## Testimonials

<div class="testimonials">
  <div class="testimonial">
    <p>"VitePress has transformed how we create and maintain our documentation. The developer experience is unmatched."</p>
    <div class="testimonial-author">
      <strong>Jane Doe</strong>
      <span>Lead Developer at Example Corp</span>
    </div>
  </div>
  
  <div class="testimonial">
    <p>"The speed and simplicity of VitePress allowed us to launch our documentation site in record time."</p>
    <div class="testimonial-author">
      <strong>John Smith</strong>
      <span>CTO at Tech Startup</span>
    </div>
  </div>
</div>

## Ready to Get Started?

<div class="cta-section">
  <h2>Start Building Your Documentation Today</h2>
  <p>Join thousands of developers who are already using VitePress for their documentation needs.</p>
  <a href="/guide/" class="cta-button">Read the Guide</a>
</div>

<style>
.feature-section {
  display: flex;
  align-items: center;
  gap: 2rem;
  margin: 2rem 0;
}

.feature-content {
  flex: 1;
}

.feature-image {
  flex: 1;
  text-align: center;
}

.feature-image img {
  max-width: 100%;
  height: auto;
}

.feature-buttons {
  display: flex;
  gap: 1rem;
  margin-top: 1.5rem;
}

.feature-button {
  display: inline-block;
  padding: 0.5rem 1.25rem;
  border-radius: 4px;
  font-weight: 500;
  text-decoration: none;
  transition: background-color 0.2s;
}

.feature-button.primary {
  background-color: var(--vp-c-brand);
  color: white;
}

.feature-button.secondary {
  background-color: #f1f1f1;
  color: #333;
}

.features-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
  gap: 1.5rem;
  margin: 2rem 0;
}

.feature-card {
  padding: 1.5rem;
  border-radius: 8px;
  background-color: #f9f9f9;
  transition: transform 0.2s, box-shadow 0.2s;
}

.feature-card:hover {
  transform: translateY(-5px);
  box-shadow: 0 5px 15px rgba(0, 0, 0, 0.1);
}

.feature-icon {
  font-size: 2rem;
  margin-bottom: 1rem;
}

.testimonials {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
  gap: 2rem;
  margin: 2rem 0;
}

.testimonial {
  padding: 1.5rem;
  border-radius: 8px;
  background-color: #f5f5f5;
  position: relative;
}

.testimonial p {
  font-style: italic;
  margin-bottom: 1rem;
}

.testimonial-author {
  display: flex;
  flex-direction: column;
}

.cta-section {
  text-align: center;
  padding: 3rem 1rem;
  margin: 3rem 0;
  background-color: #f9f9f9;
  border-radius: 8px;
}

.cta-button {
  display: inline-block;
  margin-top: 1rem;
  padding: 0.75rem 1.5rem;
  background-color: var(--vp-c-brand);
  color: white;
  border-radius: 4px;
  font-weight: 500;
  text-decoration: none;
}

@media (max-width: 768px) {
  .feature-section {
    flex-direction: column;
  }
  
  .testimonials {
    grid-template-columns: 1fr;
  }
}
</style>