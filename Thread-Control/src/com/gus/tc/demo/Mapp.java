package com.gus.tc.demo;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

public class Mapp {

	public static final Map<Integer,Thread> m = new HashMap<Integer,Thread> ();

	@SuppressWarnings("deprecation")
	public static void main(String[] args) throws InterruptedException, ExecutionException {
		// TODO Auto-generated method stub
		
		
		ExecutorService service = Executors.newFixedThreadPool(2);

		Future<String> future1 = service.submit(new Callable<String>() {
			@Override
			public String call() throws Exception {
				m.put(1, Thread.currentThread());
				
				Thread.sleep(5000);
				return "Hello1";
			}
		});

		Future<String> future2 = service.submit(new Callable<String>() {
			@Override
			public String call() throws Exception {
				m.put(2, Thread.currentThread());
				Thread.sleep(5000);
				return "Hello2";
			}
		});

		System.out.println("等待拿到结果：");
		Thread.sleep(1000);
		System.out.println(m);
		service.shutdown();
		m.get(1).stop();
		m.get(2).stop();
		
		System.out.println(future1.get());
		System.out.println(future2.get());

		
		
		
		
//		m.get(1).stop();
//		m.get(2).stop();
		
	}

}
