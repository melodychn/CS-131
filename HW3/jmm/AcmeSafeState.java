import java.util.concurrent.atomic.AtomicLongArray;
class AcmeSafeState implements State {
    private AtomicLongArray value;
    AcmeSafeState(int length) { value = new AtomicLongArray(length); }
    public int size() { return value.length(); }
    public long[] current() { 
        long[] arr = new long[size()];
        for(int k = 0; k < size(); k++)
            arr[k] = value.get(k);
        return arr; 
    }
    public void swap(int i, int j) {
        value.getAndDecrement(i);
        value.getAndIncrement(j);
    }
}