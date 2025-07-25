import { StorageManager } from './storageManager';
import { ScheduleEvent } from '../types';

describe('StorageManager', () => {
  let storageManager: StorageManager;

  const createTestEvent = (id: string): ScheduleEvent => ({
    id,
    title: `Test Event ${id}`,
    description: `Description for event ${id}`,
    startTime: new Date('2024-01-01T10:00:00'),
    endTime: new Date('2024-01-01T11:00:00'),
    createdAt: new Date(),
    updatedAt: new Date(),
    priority: 'medium'
  });

  beforeEach(() => {
    storageManager = new StorageManager();
    // Clear localStorage before each test
    localStorage.clear();
  });

  it('should be available when localStorage is supported', () => {
    expect(storageManager.isAvailable()).toBe(true);
  });

  it('should save and load events correctly', async () => {
    const testEvents = [createTestEvent('1'), createTestEvent('2')];
    
    await storageManager.save(testEvents);
    const loadedEvents = await storageManager.load();
    
    expect(loadedEvents).toHaveLength(2);
    expect(loadedEvents[0].title).toBe('Test Event 1');
    expect(loadedEvents[0].startTime).toBeInstanceOf(Date);
  });

  it('should return empty array for initial load', async () => {
    const result = await storageManager.load();
    expect(result).toEqual([]);
  });

  it('should remove event correctly', async () => {
    const testEvents = [createTestEvent('1'), createTestEvent('2')];
    
    await storageManager.save(testEvents);
    await storageManager.remove('1');
    
    const result = await storageManager.load();
    expect(result).toHaveLength(1);
    expect(result[0].id).toBe('2');
  });

  it('should clear all data', async () => {
    const testEvents = [createTestEvent('1'), createTestEvent('2')];
    
    await storageManager.save(testEvents);
    await storageManager.clear();
    
    const result = await storageManager.load();
    expect(result).toEqual([]);
  });
});