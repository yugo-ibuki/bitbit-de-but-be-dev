import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import '@testing-library/jest-dom';
import ErrorMessage from '../ErrorMessage';

describe('ErrorMessage', () => {
  it('renders error message correctly', () => {
    render(
      <ErrorMessage
        type="error"
        message="テストエラーメッセージ"
        title="エラータイトル"
      />
    );

    expect(screen.getByText('エラータイトル')).toBeInTheDocument();
    expect(screen.getByText('テストエラーメッセージ')).toBeInTheDocument();
  });

  it('renders without title', () => {
    render(
      <ErrorMessage
        type="info"
        message="情報メッセージ"
      />
    );

    expect(screen.getByText('情報メッセージ')).toBeInTheDocument();
    expect(screen.queryByText('エラータイトル')).not.toBeInTheDocument();
  });

  it('calls onClose when close button is clicked', async () => {
    const mockOnClose = jest.fn();
    
    render(
      <ErrorMessage
        type="warning"
        message="警告メッセージ"
        onClose={mockOnClose}
      />
    );

    const closeButton = screen.getByLabelText('閉じる');
    fireEvent.click(closeButton);

    // Wait for the animation delay (300ms)
    await new Promise(resolve => setTimeout(resolve, 350));

    expect(mockOnClose).toHaveBeenCalledTimes(1);
  });

  it('renders different types with appropriate styling', () => {
    const { rerender } = render(
      <ErrorMessage
        type="error"
        message="エラー"
      />
    );

    let container = screen.getByText('エラー').closest('.error-message');
    expect(container).toHaveClass('error-message--error');

    rerender(
      <ErrorMessage
        type="success"
        message="成功"
      />
    );

    container = screen.getByText('成功').closest('.error-message');
    expect(container).toHaveClass('error-message--success');
  });

  it('does not render close button when onClose is not provided', () => {
    render(
      <ErrorMessage
        type="info"
        message="情報メッセージ"
      />
    );

    expect(screen.queryByLabelText('閉じる')).not.toBeInTheDocument();
  });
});