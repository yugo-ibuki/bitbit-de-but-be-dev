import { render, screen } from '@testing-library/react'
import { App } from './App'

it('shows the product name', () => {
  render(<App />)
  expect(
    screen.getByRole('heading', { name: 'みんなのものさし' }),
  ).toBeInTheDocument()
})
