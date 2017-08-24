package cn.migu.file.core;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.BlockingQueue;
import java.util.regex.Pattern;

public class BlockingQueue4Files {

	//每一个表达式一个队列
	public static Map<Pattern, BlockingQueue<BlockingQueneFileInfo>> bqf = new HashMap<Pattern, BlockingQueue<BlockingQueneFileInfo>>();

}
