package com.gus.service;

public class Mapp2 {

	public static void main(String[] args) throws InterruptedException {
		// TODO Auto-generated method stub

		T2 t1 = new T2();

		t1.start();

		Thread.sleep(1000);

		// t1.interrupt();

		t1.stop();
		

		new Thread(new Runnable() {
			public void run() {
				for (;;) {
					try {
						Thread.sleep(1000);
					} catch (InterruptedException e) {
						e.printStackTrace();
					}
					System.out.println(1);
				}
			}
		}).start();

	}

}

class T1 extends Thread {

	public Thread tc = null;

	@Override
	public void run() {
		super.run();

		// for(int i=0;i<100000000;i++) {
		// System.out.println(i);
		// }

		Thread t = new Thread(new Runnable() {
			public Thread t2 = null;

			public void run() {
				for (int i = 0; i < 100000000; i++) {
					System.out.println(i);
				}
			}
		});
		t.start();

		tc = t;

	}
}

class T2 extends Thread {

	@Override
	public void run() {
		super.run();
		do1();
	}

	public void do1() {

		for (int i = 1000000; i < 100000000; i++) {
			System.out.println(i);
		}
	}
}