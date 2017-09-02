package com.gus.service;

import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.FutureTask;

import javax.servlet.http.HttpServletRequest;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.BatchPreparedStatementSetter;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/mydev")
public class HttpService {

	public final static Map<Long, Object> MYDEV_THREAD_INTTERUPT_FLAG = new HashMap<Long, Object>();

	@Autowired
	private JdbcTemplate jdbcTemplate;

	@Autowired
	private HttpServletRequest request;

	// MYDEV_THREAD_INTTERUPT_FLAG 保证全局唯一
	@SuppressWarnings({ "unchecked", "deprecation" })
	@RequestMapping("/s1")
	// 打断接口
	// http://127.0.0.1:8081/mydev/s1?MYDEV_THREAD_INTTERUPT_FLAG=100
	public String s1() throws Exception {

		Long id = Long.parseLong(request.getParameter("MYDEV_THREAD_INTTERUPT_FLAG"));

		if (MYDEV_THREAD_INTTERUPT_FLAG.get(id) instanceof FutureTask) {
			FutureTask<String> future = (FutureTask<String>) MYDEV_THREAD_INTTERUPT_FLAG.get(id);
			future.cancel(true);
		}

		if (MYDEV_THREAD_INTTERUPT_FLAG.get(id) instanceof Thread) {
			Thread thread = (Thread) MYDEV_THREAD_INTTERUPT_FLAG.get(id);
			thread.stop();
			// thread.interrupt();
		}

		return id + " THREAD_INTTERUPT...";
	}

	@RequestMapping("/s2")
	// 模拟爬虫接口
	// http://127.0.0.1:8081/mydev/s2?MYDEV_THREAD_INTTERUPT_FLAG=100
	public String s2() {

		Long id = Long.parseLong(request.getParameter("MYDEV_THREAD_INTTERUPT_FLAG"));
		Callable<String> callable = new Callable<String>() {
			public String call() throws Exception {
				try {
					for (int i = 0; i < 1000000000; i++) {
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

		MYDEV_THREAD_INTTERUPT_FLAG.put(id, future);

		new Thread(future).start();

		try {
			return future.get();
		} catch (InterruptedException e) {
			return e.getMessage();

		} catch (ExecutionException e) {
			return e.getMessage();
		}
	}

	@RequestMapping("/s3")
	// http://127.0.0.1:8081/mydev/s3?MYDEV_THREAD_INTTERUPT_FLAG=100
	public String s3() {

		Long id = Long.parseLong(request.getParameter("MYDEV_THREAD_INTTERUPT_FLAG"));

		MYDEV_THREAD_INTTERUPT_FLAG.put(id, Thread.currentThread());

		do1();

		return "100";
	}

	public static void do1() {
		for (int i = 0; i < 1000000000; i++) {
			System.out.println(i);
		}
	}

	
	//运行 http://127.0.0.1:8081/mydev/s4?MYDEV_THREAD_INTTERUPT_FLAG=100
	//停止 http://127.0.0.1:8081/mydev/s1?MYDEV_THREAD_INTTERUPT_FLAG=100
	
	@RequestMapping("/s4")
	public String s4() {

		Long id = Long.parseLong(request.getParameter("MYDEV_THREAD_INTTERUPT_FLAG"));

		MYDEV_THREAD_INTTERUPT_FLAG.put(id, Thread.currentThread());

		String sql = "insert into t1 values (?,?)";

		List<Foo> list = new ArrayList<Foo>();

		//批量插入1000000
		for (int i = 0; i < 1000000; i++) {

			list.add(new Foo(i, "插" + i));
		}

		jdbcTemplate.batchUpdate(sql, new BatchPreparedStatementSetter() {
			public int getBatchSize() {
				return list.size();
			}

			public void setValues(PreparedStatement ps, int i) throws SQLException {
				Foo foo = (Foo) list.get(i);
				ps.setInt(1, foo.getId());
				ps.setString(2, foo.getName());
			}
		});

		return "0000";
	}

}
