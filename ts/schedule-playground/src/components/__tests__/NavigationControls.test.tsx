import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import '@testing-library/jest-dom';
import NavigationControls from '../NavigationControls';
import { ViewType } from '../../types';

describe('NavigationControls', () => {
  const mockOnDateChange = jest.fn();
  const mockOnTodayClick = jest.fn();

  beforeEach(() => {
    jest.clearAllMocks();
  });

  const defaultProps = {
    currentDate: new Date('2024-01-15'),
    viewType: 'week' as ViewType,
    onDateChange: mockOnDateChange,
    onTodayClick: mockOnTodayClick,
  };

  it('renders navigation controls correctly', () => {
    render(<NavigationControls {...defaultProps} />);

    expect(screen.getByRole('button', { name: /前の週へ移動/ })).toBeInTheDocument();
    expect(screen.getByRole('button', { name: /次の週へ移動/ })).toBeInTheDocument();
    expect(screen.getByRole('button', { name: /今日/ })).toBeInTheDocument();
  });

  it('displays current date correctly for week view', () => {
    render(<NavigationControls {...defaultProps} />);

    // Week view should show date range
    expect(screen.getByText(/2024年1月/)).toBeInTheDocument();
  });

  it('displays current date correctly for month view', () => {
    render(<NavigationControls {...defaultProps} viewType="month" />);

    expect(screen.getByText('2024年1月')).toBeInTheDocument();
  });

  it('displays current date correctly for day view', () => {
    render(<NavigationControls {...defaultProps} viewType="day" />);

    expect(screen.getByText(/2024年1月15日/)).toBeInTheDocument();
  });

  it('calls onDateChange with previous month when previous button is clicked in month view', () => {
    render(<NavigationControls {...defaultProps} viewType="month" />);

    const prevButton = screen.getByRole('button', { name: /前の月へ移動/ });
    fireEvent.click(prevButton);

    expect(mockOnDateChange).toHaveBeenCalledWith(
      expect.objectContaining({
        getMonth: expect.any(Function),
        getFullYear: expect.any(Function),
      })
    );

    const calledDate = mockOnDateChange.mock.calls[0][0];
    expect(calledDate.getMonth()).toBe(11); // December (0-indexed)
    expect(calledDate.getFullYear()).toBe(2023);
  });

  it('calls onDateChange with next week when next button is clicked in week view', () => {
    render(<NavigationControls {...defaultProps} />);

    const nextButton = screen.getByRole('button', { name: /次の週へ移動/ });
    fireEvent.click(nextButton);

    expect(mockOnDateChange).toHaveBeenCalledWith(
      expect.objectContaining({
        getDate: expect.any(Function),
      })
    );

    const calledDate = mockOnDateChange.mock.calls[0][0];
    expect(calledDate.getDate()).toBe(22); // 7 days later
  });

  it('calls onDateChange with previous day when previous button is clicked in day view', () => {
    render(<NavigationControls {...defaultProps} viewType="day" />);

    const prevButton = screen.getByRole('button', { name: /前の日へ移動/ });
    fireEvent.click(prevButton);

    expect(mockOnDateChange).toHaveBeenCalledWith(
      expect.objectContaining({
        getDate: expect.any(Function),
      })
    );

    const calledDate = mockOnDateChange.mock.calls[0][0];
    expect(calledDate.getDate()).toBe(14); // 1 day earlier
  });

  it('calls onTodayClick when today button is clicked', () => {
    render(<NavigationControls {...defaultProps} />);

    const todayButton = screen.getByRole('button', { name: /今日/ });
    fireEvent.click(todayButton);

    expect(mockOnTodayClick).toHaveBeenCalledTimes(1);
  });

  it('disables today button when current period includes today', () => {
    const today = new Date();
    render(<NavigationControls {...defaultProps} currentDate={today} />);

    const todayButton = screen.getByRole('button', { name: /今日/ });
    expect(todayButton).toBeDisabled();
  });

  it('enables today button when current period does not include today', () => {
    const pastDate = new Date('2020-01-01');
    render(<NavigationControls {...defaultProps} currentDate={pastDate} />);

    const todayButton = screen.getByRole('button', { name: /今日/ });
    expect(todayButton).not.toBeDisabled();
  });
});