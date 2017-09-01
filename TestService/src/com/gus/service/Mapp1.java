package com.gus.service;

import java.util.Random;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.FutureTask;

public class Mapp1 {

	public static void main(String[] args) throws InterruptedException {
		// TODO Auto-generated method stub

		Callable<String> callable = new Callable<String>() {
			public String call() throws Exception {
				try {
					for (int i = 0; i < 10000000; i++) {
						System.out.println(i);
						if (Thread.currentThread().isInterrupted()) {
							throw new InterruptedException();
						}
					}
					return new Random().nextInt(1000) + "";
				} catch (InterruptedException e) {
					return "主动打断 callable=============================>>>";
				}
			}
		};

		FutureTask<String> future = new FutureTask<String>(callable);

		new Thread(future).start();

		Thread.sleep(1000);
		future.cancel(true);

		try {
			System.out.println(future.get());
		} catch (InterruptedException e) {
			e.printStackTrace();

		} catch (ExecutionException e) {
			e.printStackTrace();
		}
	}
}
