export interface Task {
  id: string;
  title: string;
  description?: string;
  startTime: Date;
  endTime: Date;
  priority: 'low' | 'medium' | 'high';
  completed: boolean;
  category?: string;
}

export interface Event {
  id: string;
  title: string;
  description?: string;
  date: Date;
  time: string;
  duration: number;
  type: 'meeting' | 'appointment' | 'reminder' | 'other';
}

export interface CalendarDay {
  date: Date;
  tasks: Task[];
  events: Event[];
  isToday: boolean;
  isSelected: boolean;
}

// Schedule Timeline Service Types
// These types define the core data structures for the timeline-based schedule management system

export type ViewType = 'month' | 'week' | 'day';

export interface ScheduleEvent {
  id: string;
  title: string;
  description: string;
  startTime: Date;
  endTime: Date;
  // Optional fields for extensibility
  color?: string;
  category?: string;
  isAllDay?: boolean;
  priority?: 'low' | 'medium' | 'high';
  location?: string;
  attendees?: string[];
  tags?: string[];
  customFields?: Record<string, any>;
  createdAt: Date;
  updatedAt: Date;
}

export interface TimeSlot {
  start: Date;
  end: Date;
  viewType: ViewType;
  gridPosition: {
    row: number;
    column: number;
  };
}

export interface DateRange {
  start: Date;
  end: Date;
}

export interface GridPosition {
  row: number;
  column: number;
  rowSpan?: number;
  columnSpan?: number;
}

export interface ViewConfiguration {
  month: {
    daysPerRow: 7;
    weeksToShow: 6;
    showTimeSlots: false;
  };
  week: {
    daysPerRow: 7;
    hoursPerDay: 24;
    timeSlotDuration: 60; // minutes
  };
  day: {
    hoursPerDay: 24;
    timeSlotDuration: 30; // minutes
    showHalfHours: true;
  };
}

// Integration types for existing Task system
export interface TaskScheduleEvent extends Omit<ScheduleEvent, 'description'> {
  description?: string;
  taskId?: string; // Reference to existing Task
  completed?: boolean; // Integration with Task completion status
}

// Utility types for timeline operations
export interface TimelineState {
  currentDate: Date;
  viewType: ViewType;
  events: ScheduleEvent[];
  selectedEvent: ScheduleEvent | null;
}

export interface EventFormData {
  title: string;
  description: string;
  startTime: Date;
  endTime: Date;
  color?: string;
  category?: string;
  isAllDay?: boolean;
  priority?: 'low' | 'medium' | 'high';
  location?: string;
  attendees?: string[];
  tags?: string[];
}

// Error handling types
export type ErrorType = 'warning' | 'error' | 'info';



export interface StorageError {
  type: 'quota_exceeded' | 'access_denied' | 'unavailable';
  message: string;
}

export interface UIError {
  component: string;
  action: string;
  message: string;
}

// Future extensibility - Recurrence support
export interface RecurrenceRule {
  frequency: 'daily' | 'weekly' | 'monthly' | 'yearly';
  interval: number;
  endDate?: Date;
  count?: number;
}